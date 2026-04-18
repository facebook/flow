/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The Flow server monitor needs to deal with multiple connections. There's the connection to the
// server (a pipe), there's 0 or more connections to ephemeral clients (over a socket), and there's
// 0 or more connections to persistent clients (over a socket). We need to be able to read and write
// to all of these clients in a non-blocking and race-condition free manner.
//
// This module wraps a connection. The basic idea is as follows:
//
// 1. There is a loop which reads from the connection. When a message is received, we pass it to the
//    on_read callback
// 2. There is a stream of things to write to the connection. Other threads can add things to this
//    stream
// 3. There is a loop which reads from the stream and writes to the connection
//
// We didn't **really** need to make this a functor. CONNECTION.t could in theory be parameterized.
// However, glevi liked writing `ServerConnection.write conn` over `Connection.write conn`. So it's
// a minor stylistic choice.

use std::fs::File;
use std::io;
use std::sync::Arc;
use std::sync::Mutex;

use serde::Serialize;
use serde::de::DeserializeOwned;
use tokio::sync::Notify;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;

use crate::runtime;

pub trait ConnectionProcessor: Send + Sync + 'static {
    type InMessage: Send + DeserializeOwned + 'static;
    type OutMessage: Send + Serialize + 'static;
}

enum Command<OutMessage> {
    Write(OutMessage),
    WriteAndClose(OutMessage),
}
pub struct Connection<P: ConnectionProcessor> {
    name: String,
    in_fd: Arc<Mutex<File>>,
    out_fd: Arc<Mutex<File>>,
    command_stream: Mutex<Option<mpsc::UnboundedReceiver<Command<P::OutMessage>>>>,
    push_to_stream: Mutex<Option<mpsc::UnboundedSender<Command<P::OutMessage>>>>,
    close: Arc<dyn Fn() + Send + Sync>,
    on_read: Arc<dyn Fn(P::InMessage, &Arc<Connection<P>>) + Send + Sync>,
    read_thread: Mutex<Option<JoinHandle<()>>>,
    command_thread: Mutex<Option<JoinHandle<()>>>,
    wait_for_closed_thread: Arc<Notify>,
    closed: Arc<std::sync::atomic::AtomicBool>,
}
fn send_command<P: ConnectionProcessor>(
    conn: &Connection<P>,
    command: Command<P::OutMessage>,
) -> bool {
    let push = conn.push_to_stream.lock().unwrap();
    match push.as_ref() {
        Some(sender) => sender.send(command).is_ok(),
        None => false,
    }
}

fn close_stream<P: ConnectionProcessor>(conn: &Connection<P>) {
    let mut push = conn.push_to_stream.lock().unwrap();
    *push = None;
    conn.closed.store(true, std::sync::atomic::Ordering::SeqCst);
}

impl<P: ConnectionProcessor> Connection<P> {
    pub fn write(&self, msg: P::OutMessage) -> bool {
        send_command(self, Command::Write(msg))
    }

    pub fn write_and_close(&self, msg: P::OutMessage) -> bool {
        let result = send_command(self, Command::WriteAndClose(msg));
        close_stream(self);
        result
    }

    // Doesn't actually close the file descriptors, but does stop all the loops and streams
    fn stop_everything(&self) {
        close_stream(self);
        if let Some(h) = self.read_thread.lock().unwrap().take() {
            h.abort();
        }
        if let Some(h) = self.command_thread.lock().unwrap().take() {
            h.abort();
        }
    }

    pub fn close_immediately(&self) {
        self.stop_everything();
        (self.close)();
    }

    fn handle_command(self: &Arc<Self>, command: Command<P::OutMessage>) -> io::Result<()> {
        match command {
            Command::Write(msg) => {
                let mut out_fd = self.out_fd.lock().unwrap();
                flow_parser::loc::with_full_source_serde(|| {
                    bincode::serialize_into(&mut *out_fd, &msg)
                        .map_err(|e| io::Error::other(e.to_string()))
                })?;
                Ok(())
            }
            Command::WriteAndClose(msg) => {
                if let Some(h) = self.command_thread.lock().unwrap().take() {
                    h.abort();
                }
                {
                    let mut out_fd = self.out_fd.lock().unwrap();
                    flow_parser::loc::with_full_source_serde(|| {
                        bincode::serialize_into(&mut *out_fd, &msg)
                            .map_err(|e| io::Error::other(e.to_string()))
                    })?;
                }
                self.close_immediately();
                Ok(())
            }
        }
    }

    // Attempts to write everything available in the stream and then close the connection,
    // but it's ok if we can't write because the socket is already closed.
    pub fn try_flush_and_close(self: &Arc<Self>) {
        self.stop_everything();
        let mut stream_slot = self.command_stream.lock().unwrap();
        if let Some(mut rx) = stream_slot.take() {
            drop(stream_slot);
            while let Ok(command) = rx.try_recv() {
                match self.handle_command(command) {
                    Ok(()) => {}
                    // connection already closed, ignore
                    Err(e) if matches!(e.kind(), io::ErrorKind::BrokenPipe) => {
                        break;
                    }
                    Err(e) => {
                        (self.close)();
                        log::error!("Connection '{}' failed to flush: {}", self.name, e);
                        return;
                    }
                }
            }
        }
        (self.close)();
    }

    pub fn is_closed(&self) -> bool {
        self.closed.load(std::sync::atomic::Ordering::SeqCst)
    }

    pub fn wait_for_closed(&self) {
        let notify = self.wait_for_closed_thread.clone();
        runtime::handle().block_on(async move {
            notify.notified().await;
        });
    }
}

mod command_loop {
    use super::*;

    enum LoopExn {
        StreamEmpty,
        Other(String),
    }

    async fn main<P: ConnectionProcessor>(
        conn: &Arc<Connection<P>>,
        rx: &mut mpsc::UnboundedReceiver<Command<P::OutMessage>>,
    ) -> Result<(), LoopExn> {
        let command = rx.recv().await.ok_or(LoopExn::StreamEmpty)?;
        let conn_for_block = conn.clone();
        let result = tokio::task::spawn_blocking(move || conn_for_block.handle_command(command))
            .await
            .map_err(|e| LoopExn::Other(format!("join error: {}", e)))?;
        match result {
            Ok(()) => Ok(()),
            Err(e) => Err(LoopExn::Other(e.to_string())),
        }
    }

    fn catch<P: ConnectionProcessor>(conn: &Arc<Connection<P>>, exn: LoopExn) {
        match exn {
            LoopExn::StreamEmpty => {}
            LoopExn::Other(e) => {
                log::error!(
                    "Closing connection '{}' due to uncaught exception in command loop: {}",
                    conn.name,
                    e
                );
                conn.close_immediately();
            }
        }
    }

    pub(super) async fn run<P: ConnectionProcessor>(
        conn: Arc<Connection<P>>,
        mut rx: mpsc::UnboundedReceiver<Command<P::OutMessage>>,
    ) {
        loop {
            tokio::task::yield_now().await;
            match main(&conn, &mut rx).await {
                Ok(()) => continue,
                Err(e) => {
                    catch(&conn, e);
                    return;
                }
            }
        }
    }
}

mod read_loop {
    use super::*;

    enum LoopExn {
        EndOfFile,
        ConnReset,
        Other(String),
    }

    fn classify_io(e: &io::Error) -> LoopExn {
        match e.kind() {
            io::ErrorKind::UnexpectedEof => LoopExn::EndOfFile,
            io::ErrorKind::ConnectionReset => LoopExn::ConnReset,
            _ => LoopExn::Other(e.to_string()),
        }
    }

    async fn main<P: ConnectionProcessor>(connection: &Arc<Connection<P>>) -> Result<(), LoopExn> {
        let in_fd = connection.in_fd.clone();
        let msg_result = tokio::task::spawn_blocking(move || -> io::Result<P::InMessage> {
            let mut in_fd = in_fd.lock().unwrap();
            flow_parser::loc::with_full_source_serde(|| {
                bincode::deserialize_from(&mut *in_fd).map_err(|e| match *e {
                    bincode::ErrorKind::Io(io_err) => io_err,
                    other => io::Error::other(other.to_string()),
                })
            })
        })
        .await
        .map_err(|e| LoopExn::Other(format!("join error: {}", e)))?;
        let msg = msg_result.map_err(|e| classify_io(&e))?;
        (connection.on_read)(msg, connection);
        Ok(())
    }

    fn catch<P: ConnectionProcessor>(connection: &Arc<Connection<P>>, exn: LoopExn) {
        match exn {
            LoopExn::EndOfFile | LoopExn::ConnReset => {
                log::error!(
                    "Connection '{}' was closed from the other side",
                    connection.name
                );
            }
            LoopExn::Other(e) => {
                log::error!(
                    "Closing connection '{}' due to uncaught exception in read loop: {}",
                    connection.name,
                    e
                );
            }
        }
        connection.close_immediately();
    }

    pub(super) async fn run<P: ConnectionProcessor>(connection: Arc<Connection<P>>) {
        loop {
            tokio::task::yield_now().await;
            match main(&connection).await {
                Ok(()) => continue,
                Err(e) => {
                    catch(&connection, e);
                    return;
                }
            }
        }
    }
}

impl<P: ConnectionProcessor> Connection<P> {
    pub fn create(
        name: String,
        in_fd: std::os::fd::OwnedFd,
        out_fd: std::os::fd::OwnedFd,
        close: impl Fn() + Send + Sync + 'static,
        on_read: impl Fn(P::InMessage, &Arc<Connection<P>>) + Send + Sync + 'static,
    ) -> (Box<dyn FnOnce() + Send>, Arc<Connection<P>>) {
        let wait_for_closed_thread = Arc::new(Notify::new());
        let wakener = wait_for_closed_thread.clone();
        let close = Arc::new(move || {
            close();
            wakener.notify_waiters();
        }) as Arc<dyn Fn() + Send + Sync>;

        let (push_to_stream, command_stream) = mpsc::unbounded_channel();

        // Lwt.task creates a thread that can be canceled
        let (start_tx_command, start_rx_command) =
            tokio::sync::oneshot::channel::<Arc<Connection<P>>>();
        let (start_tx_read, start_rx_read) = tokio::sync::oneshot::channel::<Arc<Connection<P>>>();

        let conn = Arc::new(Connection {
            name,
            in_fd: Arc::new(Mutex::new(File::from(in_fd))),
            out_fd: Arc::new(Mutex::new(File::from(out_fd))),
            command_stream: Mutex::new(Some(command_stream)),
            push_to_stream: Mutex::new(Some(push_to_stream)),
            close,
            on_read: Arc::new(on_read),
            read_thread: Mutex::new(None),
            command_thread: Mutex::new(None),
            wait_for_closed_thread,
            closed: Arc::new(std::sync::atomic::AtomicBool::new(false)),
        });

        let command_thread = runtime::handle().spawn(async move {
            let conn = match start_rx_command.await {
                Ok(c) => c,
                Err(_) => return,
            };
            let rx = match conn.command_stream.lock().unwrap().take() {
                Some(rx) => rx,
                None => return,
            };
            command_loop::run(conn, rx).await;
        });
        *conn.command_thread.lock().unwrap() = Some(command_thread);

        let read_thread = runtime::handle().spawn(async move {
            let conn = match start_rx_read.await {
                Ok(c) => c,
                Err(_) => return,
            };
            read_loop::run(conn).await;
        });
        *conn.read_thread.lock().unwrap() = Some(read_thread);

        let conn_for_start = conn.clone();
        let start: Box<dyn FnOnce() + Send> = Box::new(move || {
            if start_tx_command.send(conn_for_start.clone()).is_err() {
                log::debug!("CommandLoop task exited before start()");
            }
            if start_tx_read.send(conn_for_start).is_err() {
                log::debug!("ReadLoop task exited before start()");
            }
        });

        (start, conn)
    }
}
pub struct EphemeralConnectionProcessor;
impl ConnectionProcessor for EphemeralConnectionProcessor {
    type InMessage = flow_server_env::server_command_with_context::ServerCommandWithContext;
    type OutMessage = flow_server_env::monitor_prot::MonitorToClientMessage;
}
pub type EphemeralConnection = Connection<EphemeralConnectionProcessor>;

pub struct PersistentConnectionProcessor;
impl ConnectionProcessor for PersistentConnectionProcessor {
    type InMessage = flow_server_env::lsp_prot::RequestWithMetadata;
    type OutMessage = flow_server_env::lsp_prot::MessageFromServer;
}
pub type MonitorPersistentConnection = Connection<PersistentConnectionProcessor>;

pub struct ServerConnectionProcessor;
impl ConnectionProcessor for ServerConnectionProcessor {
    type InMessage = flow_server_env::monitor_prot::ServerToMonitorMessage;
    type OutMessage = flow_server_env::monitor_prot::MonitorToServerMessage;
}
pub type ServerConnection = Connection<ServerConnectionProcessor>;
