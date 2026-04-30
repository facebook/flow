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

use std::io;
use std::sync::Arc;
use std::sync::Mutex;

use bincode::error::DecodeError;
use tokio::sync::Notify;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;

use crate::runtime;

// A `Channel` abstracts the wire I/O for a `Connection`. Each `ConnectionProcessor` chooses its
// channel. All Flow monitor channels are bincode-framed (OCaml-faithful, matching
// `Marshal_tools.{from,to}_fd_with_preamble`):
//   - `BincodeChannel<File, _, _>`: monitor↔server pipe.
//   - `BincodeChannel<TcpStream, ServerCommandWithContext, MonitorToClientMessage>`:
//     ephemeral CLI socket.
//   - `PersistentBincodeChannel`: persistent LSP socket. The persistent path layers an outbound
//     queue on top of bincode reads to match the OCaml LSP poll/queue contract.
pub trait Channel: Send + Sync + 'static {
    type Reader: Send + 'static;
    type Writer: Send + 'static;
    type InMessage: Send + 'static;
    type OutMessage: Send + 'static;

    fn read(
        reader: &mut Self::Reader,
    ) -> io::Result<ReadOutcome<Self::InMessage, Self::OutMessage>>;
    fn write(writer: &mut Self::Writer, msg: &Self::OutMessage) -> io::Result<()>;
}

// A read either yields a logical inbound message for `on_read`, or for some channels (the
// persistent JSON-RPC channel in particular) it can produce a synchronous reply that must be
// written back without going through `on_read` (e.g. `PersistentPoll`). `Skip` lets the channel
// silently drop wire frames that have no logical mapping.
pub enum ReadOutcome<I, O> {
    Message(I),
    Reply(O),
    Skip,
}

pub trait ConnectionProcessor: Send + Sync + 'static {
    type Ch: Channel;
}

// Convenience aliases so existing code can continue to write `P::InMessage`/`P::OutMessage`.
pub trait ProcessorTypes {
    type InMessage: Send + 'static;
    type OutMessage: Send + 'static;
}
impl<P: ConnectionProcessor> ProcessorTypes for P {
    type InMessage = <P::Ch as Channel>::InMessage;
    type OutMessage = <P::Ch as Channel>::OutMessage;
}

enum Command<OutMessage> {
    Write(OutMessage),
    WriteAndClose(OutMessage),
}
pub struct Connection<P: ConnectionProcessor> {
    name: String,
    reader: Arc<Mutex<<P::Ch as Channel>::Reader>>,
    writer: Arc<Mutex<<P::Ch as Channel>::Writer>>,
    command_stream: Mutex<Option<mpsc::UnboundedReceiver<Command<<P::Ch as Channel>::OutMessage>>>>,
    push_to_stream: Mutex<Option<mpsc::UnboundedSender<Command<<P::Ch as Channel>::OutMessage>>>>,
    close: Arc<dyn Fn() + Send + Sync>,
    on_read: Arc<dyn Fn(<P::Ch as Channel>::InMessage, &Arc<Connection<P>>) + Send + Sync>,
    read_thread: Mutex<Option<JoinHandle<()>>>,
    command_thread: Mutex<Option<JoinHandle<()>>>,
    wait_for_closed_thread: Arc<Notify>,
    closed: Arc<std::sync::atomic::AtomicBool>,
}
fn send_command<P: ConnectionProcessor>(
    conn: &Connection<P>,
    command: Command<<P::Ch as Channel>::OutMessage>,
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
    pub fn write(&self, msg: <P::Ch as Channel>::OutMessage) -> bool {
        send_command(self, Command::Write(msg))
    }

    pub fn write_and_close(&self, msg: <P::Ch as Channel>::OutMessage) -> bool {
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

    fn handle_command(
        self: &Arc<Self>,
        command: Command<<P::Ch as Channel>::OutMessage>,
    ) -> io::Result<()> {
        match command {
            Command::Write(msg) => {
                let mut writer = self.writer.lock().unwrap();
                P::Ch::write(&mut *writer, &msg)?;
                Ok(())
            }
            Command::WriteAndClose(msg) => {
                if let Some(h) = self.command_thread.lock().unwrap().take() {
                    h.abort();
                }
                {
                    let mut writer = self.writer.lock().unwrap();
                    P::Ch::write(&mut *writer, &msg)?;
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

    // Returns the underlying Notify so an async caller can `await` connection-close
    // directly inside the shared runtime.
    pub fn wait_for_closed_notify(&self) -> Arc<Notify> {
        self.wait_for_closed_thread.clone()
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
        rx: &mut mpsc::UnboundedReceiver<Command<<P::Ch as Channel>::OutMessage>>,
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
        mut rx: mpsc::UnboundedReceiver<Command<<P::Ch as Channel>::OutMessage>>,
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
        let reader = connection.reader.clone();
        let outcome_result = tokio::task::spawn_blocking(
            move || -> io::Result<
                ReadOutcome<<P::Ch as Channel>::InMessage, <P::Ch as Channel>::OutMessage>,
            > {
                let mut reader = reader.lock().unwrap();
                P::Ch::read(&mut *reader)
            },
        )
        .await
        .map_err(|e| LoopExn::Other(format!("join error: {}", e)))?;
        let outcome = outcome_result.map_err(|e| classify_io(&e))?;
        match outcome {
            ReadOutcome::Message(msg) => {
                (connection.on_read)(msg, connection);
            }
            ReadOutcome::Reply(reply) => {
                if !connection.write(reply) {
                    return Err(LoopExn::EndOfFile);
                }
            }
            ReadOutcome::Skip => {}
        }
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
        reader: <P::Ch as Channel>::Reader,
        writer: <P::Ch as Channel>::Writer,
        close: impl Fn() + Send + Sync + 'static,
        on_read: impl Fn(<P::Ch as Channel>::InMessage, &Arc<Connection<P>>) + Send + Sync + 'static,
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
            reader: Arc::new(Mutex::new(reader)),
            writer: Arc::new(Mutex::new(writer)),
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

// ---- Channels ----

// PipeBincodeChannel: used by ServerConnection (monitor↔server pipe). Each frame is a
// bincode-serialized value over a TcpStream-backed transport from `flow_daemon`,
// matching OCaml `Marshal_tools.{from,to}_fd_with_preamble`. The underlying
// transport is `TcpStream` (not `File`) so that this channel works on both
// Unix and Windows -- on Windows, sockets are not files.
pub struct PipeBincodeChannel<I, O>(std::marker::PhantomData<(I, O)>);

impl<I, O> Channel for PipeBincodeChannel<I, O>
where
    I: Send + Sync + serde::de::DeserializeOwned + 'static,
    O: Send + Sync + serde::Serialize + 'static,
{
    type Reader = std::net::TcpStream;
    type Writer = std::net::TcpStream;
    type InMessage = I;
    type OutMessage = O;

    fn read(reader: &mut Self::Reader) -> io::Result<ReadOutcome<I, O>> {
        flow_parser::loc::with_full_source_serde(|| {
            bincode::serde::decode_from_std_read(reader, bincode::config::legacy())
                .map(ReadOutcome::Message)
                .map_err(|e| match e {
                    DecodeError::Io {
                        inner: io_err,
                        additional: _,
                    } => io_err,
                    other => io::Error::other(other.to_string()),
                })
        })
    }

    fn write(writer: &mut Self::Writer, msg: &O) -> io::Result<()> {
        flow_parser::loc::with_full_source_serde(|| {
            match bincode::serde::encode_into_std_write(msg, writer, bincode::config::legacy()) {
                Ok(_) => Ok(()),
                Err(e) => Err(io::Error::other(e.to_string())),
            }
        })
    }
}

// TcpBincodeChannel: used by ephemeral CLI sockets. Each frame is a bincode-serialized value over
// a TcpStream, matching OCaml `Marshal_tools.{from,to}_fd_with_preamble`.
pub struct TcpBincodeChannel<I, O>(std::marker::PhantomData<(I, O)>);

impl<I, O> Channel for TcpBincodeChannel<I, O>
where
    I: Send + Sync + serde::de::DeserializeOwned + 'static,
    O: Send + Sync + serde::Serialize + 'static,
{
    type Reader = std::net::TcpStream;
    type Writer = std::net::TcpStream;
    type InMessage = I;
    type OutMessage = O;

    fn read(reader: &mut Self::Reader) -> io::Result<ReadOutcome<I, O>> {
        flow_parser::loc::with_full_source_serde(|| {
            bincode::serde::decode_from_std_read(reader, bincode::config::legacy())
                .map(ReadOutcome::Message)
                .map_err(|e| match e {
                    DecodeError::Io {
                        inner: io_err,
                        additional: _,
                    } => io_err,
                    other => io::Error::other(other.to_string()),
                })
        })
    }

    fn write(writer: &mut Self::Writer, msg: &O) -> io::Result<()> {
        flow_parser::loc::with_full_source_serde(|| {
            match bincode::serde::encode_into_std_write(msg, writer, bincode::config::legacy()) {
                Ok(_) => Ok(()),
                Err(e) => Err(io::Error::other(e.to_string())),
            }
        })
    }
}

// PersistentBincodeChannel: used by persistent LSP sockets.
//
// The persistent connection in OCaml exchanges `LspProt.request_with_metadata` (client→server) and
// `LspProt.message_from_server` (server→client) directly over the socket via Marshal_tools (see
// `flow/src/monitor/connections/persistentConnection.ml`). The Rust port keeps an outbound queue
// inside `PersistentWriter` so that `Connection`'s command loop can drain it on the writer thread,
// matching how OCaml's `EphemeralConnection.write` enqueues into the connection's command stream.
pub struct PersistentBincodeChannel;

impl Channel for PersistentBincodeChannel {
    type Reader = PersistentReader;
    type Writer = PersistentWriter;
    type InMessage = flow_server_env::lsp_prot::RequestWithMetadata;
    type OutMessage = flow_server_env::lsp_prot::MessageFromServer;

    fn read(
        reader: &mut Self::Reader,
    ) -> io::Result<ReadOutcome<Self::InMessage, Self::OutMessage>> {
        flow_parser::loc::with_full_source_serde(|| {
            bincode::serde::decode_from_std_read(&mut reader.stream, bincode::config::legacy())
                .map(ReadOutcome::Message)
                .map_err(|e| match e {
                    DecodeError::Io {
                        inner: io_err,
                        additional: _,
                    } => io_err,
                    other => io::Error::other(other.to_string()),
                })
        })
    }

    fn write(writer: &mut Self::Writer, msg: &Self::OutMessage) -> io::Result<()> {
        flow_parser::loc::with_full_source_serde(|| {
            match bincode::serde::encode_into_std_write(
                msg,
                &mut writer.stream,
                bincode::config::legacy(),
            ) {
                Ok(_) => Ok(()),
                Err(e) => Err(io::Error::other(e.to_string())),
            }
        })
    }
}

pub struct PersistentReader {
    pub stream: std::net::TcpStream,
    pub disconnect: Arc<dyn Fn() + Send + Sync>,
}

pub struct PersistentWriter {
    pub stream: std::net::TcpStream,
}

pub struct EphemeralConnectionProcessor;
impl ConnectionProcessor for EphemeralConnectionProcessor {
    type Ch = TcpBincodeChannel<
        flow_server_env::server_command_with_context::ServerCommandWithContext,
        flow_server_env::monitor_prot::MonitorToClientMessage,
    >;
}
pub type EphemeralConnection = Connection<EphemeralConnectionProcessor>;

pub struct PersistentConnectionProcessor;
impl ConnectionProcessor for PersistentConnectionProcessor {
    type Ch = PersistentBincodeChannel;
}
pub type MonitorPersistentConnection = Connection<PersistentConnectionProcessor>;

pub struct ServerConnectionProcessor;
impl ConnectionProcessor for ServerConnectionProcessor {
    type Ch = PipeBincodeChannel<
        flow_server_env::monitor_prot::ServerToMonitorMessage,
        flow_server_env::monitor_prot::MonitorToServerMessage,
    >;
}
pub type ServerConnection = Connection<ServerConnectionProcessor>;
