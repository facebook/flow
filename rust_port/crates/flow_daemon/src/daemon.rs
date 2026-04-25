/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::marker::PhantomData;
use std::net::TcpListener;
use std::net::TcpStream;
use std::path::Path;
use std::process::Child;
use std::process::Command;
use std::process::Stdio;
use std::time::Duration;

use flow_common::sys_utils;
use serde::Serialize;
use serde::de::DeserializeOwned;

use crate::pid_log;

pub struct InChannel<T> {
    stream: TcpStream,
    _phantom: PhantomData<fn() -> T>,
}

// type 'a out_channel = Stdlib.out_channel
pub struct OutChannel<T> {
    stream: TcpStream,
    _phantom: PhantomData<fn(T)>,
}

pub struct ChannelPair<In, Out>(pub InChannel<In>, pub OutChannel<Out>);

pub struct Handle<In, Out> {
    pub channels: ChannelPair<In, Out>,
    pub child: Child,
}

pub enum ChannelMode {
    Pipe,
    Socket,
}

pub fn to_channel<T: Serialize>(oc: &mut OutChannel<T>, v: &T, should_flush: bool) {
    bincode::serde::encode_into_std_write(v, &mut oc.stream, bincode::config::legacy())
        .expect("Daemon::to_channel: bincode serialize");
    if should_flush {
        oc.stream.flush().expect("Daemon::to_channel: flush failed");
    }
}

pub fn from_channel<T: DeserializeOwned>(ic: &mut InChannel<T>, timeout: Option<Duration>) -> T {
    try_from_channel(ic, timeout).expect("Daemon::from_channel: bincode deserialize")
}

pub fn try_from_channel<T: DeserializeOwned>(
    ic: &mut InChannel<T>,
    timeout: Option<Duration>,
) -> Result<T, bincode::error::DecodeError> {
    ic.stream
        .set_read_timeout(timeout)
        .map_err(|e| bincode::error::DecodeError::Io {
            inner: e,
            additional: 0,
        })?;
    bincode::serde::decode_from_std_read(&mut ic.stream, bincode::config::legacy())
}

pub fn flush<T>(oc: &mut OutChannel<T>) {
    oc.stream.flush().expect("Daemon::flush failed");
}

// OCaml's `Unix.file_descr` is uniformly an int (or HANDLE on Windows),
// allowing it to refer to either a file or a socket. Rust has no such
// uniform type that is also cross-platform, so for `InChannel`/`OutChannel`
// (which are always TCP sockets in this port) we expose the underlying
// `TcpStream` directly. Callers that need an independent handle can call
// `try_clone()` on the returned reference -- this works on both Unix and
// Windows, unlike `BorrowedFd`/`nix::unistd::dup` which are Unix-only.
pub fn descr_of_in_channel<T>(ic: &InChannel<T>) -> &TcpStream {
    &ic.stream
}

pub fn descr_of_out_channel<T>(oc: &OutChannel<T>) -> &TcpStream {
    &oc.stream
}

pub fn into_out_writer<T>(oc: OutChannel<T>) -> Box<dyn std::io::Write + Send> {
    Box::new(oc.stream)
}

pub fn into_in_reader<T>(ic: InChannel<T>) -> Box<dyn std::io::Read + Send> {
    Box::new(ic.stream)
}

pub fn shutdown_out_write<T>(oc: &mut OutChannel<T>) -> std::io::Result<()> {
    oc.stream.shutdown(std::net::Shutdown::Write)
}

// Channel-to-stream conversions: the underlying transport is always a
// `TcpStream`, so we expose that directly rather than coercing through
// `OwnedFd`. Forcing the conversion through `OwnedFd::from(stream)` was
// Unix-only because `OwnedFd` does not exist on Windows (sockets there are
// `OwnedSocket`s, not file descriptors). `TcpStream` itself is
// cross-platform and implements both `Read` and `Write`, so callers can
// keep using it for bincode framing.
pub fn into_in_stream<T>(ic: InChannel<T>) -> TcpStream {
    ic.stream
}

pub fn into_out_stream<T>(oc: OutChannel<T>) -> TcpStream {
    oc.stream
}

// We cannot fork() on Windows, so in order to emulate this in a
// cross-platform way, we use create_process() and set the HH_SERVER_DAEMON
// environment variable to indicate which function the child should
// execute. On Unix, create_process() does fork + exec, so global state is
// not copied; in particular, if you have set a mutable reference the
// daemon will not see it. All state must be explicitly passed via
// environment variables; see set/get_context() below.
//
// With some factoring we could make the daemons into separate binaries
// altogether and dispense with this emulation.

pub mod entry {
    use std::collections::HashMap;
    use std::fs;
    use std::io::Write;
    use std::marker::PhantomData;
    use std::net::SocketAddr;
    use std::path::PathBuf;
    use std::sync::Mutex;
    use std::sync::OnceLock;

    use flow_common::sys_utils;
    use serde::Deserialize;
    use serde::Serialize;
    use serde::de::DeserializeOwned;
    use tempfile::NamedTempFile;

    use super::ChannelPair;
    use super::InChannel;
    use super::OutChannel;

    pub struct Entry<Param, Input, Output> {
        pub(super) name: &'static str,
        pub(super) _phantom: PhantomData<fn(Param, Input, Output)>,
    }

    pub fn name_of_entry<P, I, O>(entry: &Entry<P, I, O>) -> &'static str {
        entry.name
    }

    type ErasedDispatch =
        Box<dyn Fn(Vec<u8>, std::net::TcpStream, std::net::TcpStream) + Send + Sync + 'static>;

    fn entry_points() -> &'static Mutex<HashMap<&'static str, ErasedDispatch>> {
        static EP: OnceLock<Mutex<HashMap<&'static str, ErasedDispatch>>> = OnceLock::new();
        EP.get_or_init(|| Mutex::new(HashMap::new()))
    }

    pub fn register<P, I, O>(name: &'static str, f: fn(P, ChannelPair<I, O>)) -> Entry<P, I, O>
    where
        P: Serialize + DeserializeOwned + Send + 'static,
        I: Serialize + DeserializeOwned + Send + 'static,
        O: Serialize + DeserializeOwned + Send + 'static,
    {
        let dispatch: ErasedDispatch = Box::new(move |bytes, in_sock, out_sock| {
            let (param, _): (P, usize) =
                bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                    .unwrap_or_else(|e| {
                        panic!("Daemon entry {:?}: param decode failed: {}", name, e)
                    });
            let in_chan = InChannel {
                stream: in_sock,
                _phantom: PhantomData,
            };
            let out_chan = OutChannel {
                stream: out_sock,
                _phantom: PhantomData,
            };
            f(param, ChannelPair(in_chan, out_chan));
        });
        let mut table = entry_points()
            .lock()
            .expect("Daemon entry_points mutex poisoned");
        if table.contains_key(name) {
            panic!(
                "Daemon.register_entry_point: duplicate entry point {:?}.",
                name
            );
        }
        table.insert(name, dispatch);
        Entry {
            name,
            _phantom: PhantomData,
        }
    }

    // Returned as a clone-by-arc-style "find and call" so we don't have to
    // hold the registry lock across the call to user code. We arrange for
    // the dispatcher to be invokable through the lock then take it out.
    pub(crate) fn find_and_call(
        name: &str,
        bytes: Vec<u8>,
        in_sock: std::net::TcpStream,
        out_sock: std::net::TcpStream,
    ) {
        let table = entry_points()
            .lock()
            .expect("Daemon entry_points mutex poisoned");
        let dispatch = table
            .get(name)
            .unwrap_or_else(|| panic!("Unknown entry point {:?}", name));
        dispatch(bytes, in_sock, out_sock);
    }

    pub(crate) const ENV_DAEMON: &str = "HH_SERVER_DAEMON";
    pub(crate) const ENV_DAEMON_PARAM: &str = "HH_SERVER_DAEMON_PARAM";

    #[derive(Serialize, Deserialize)]
    pub(crate) struct Context {
        // Listener address corresponding to the parent's `InChannel` (the
        // parent reads from this end of the TCP connection). The child must
        // connect to this addr and *write* to it -- it is the child's
        // outgoing channel.
        pub parent_in_addr: SocketAddr,
        // Listener address corresponding to the parent's `OutChannel` (the
        // parent writes to this end). The child must connect to this addr
        // and *read* from it -- it is the child's incoming channel.
        pub parent_out_addr: SocketAddr,
        pub token: [u8; 32],
        pub param_bytes: Vec<u8>,
    }

    pub(crate) fn write_param_file<P: Serialize>(
        parent_in_addr: SocketAddr,
        parent_out_addr: SocketAddr,
        token: [u8; 32],
        param: &P,
    ) -> std::io::Result<PathBuf> {
        let param_bytes = bincode::serde::encode_to_vec(param, bincode::config::legacy())
            .expect("Daemon::set_context: bincode serialize param");
        let context = Context {
            parent_in_addr,
            parent_out_addr,
            token,
            param_bytes,
        };
        // Include the PID in the prefix so that forked processes (which share
        // OCaml's internal Filename PRNG state) generate distinct temp names.
        let prefix = format!("daemon_param_{}_", std::process::id());
        let temp_dir = sys_utils::temp_dir_name();
        let temp_file = NamedTempFile::with_prefix_in(&prefix, &temp_dir)?;
        let path = temp_file.path().to_owned();
        // Use `persist` to keep the file after `temp_file` drops; the child
        // is responsible for deleting it (mirrors `daemon.ml:122
        // `Sys.remove file` in `get_context`).
        let (mut file, _path) = temp_file.keep().map_err(|e| e.error)?;
        bincode::serde::encode_into_std_write(&context, &mut file, bincode::config::legacy())
            .expect("Daemon::set_context: bincode serialize context");
        file.flush()?;
        Ok(path)
    }

    // How this works on Unix: It may appear like we are passing file descriptors
    // from one process to another here, but in_handle / out_handle are actually
    // file descriptors that are already open in the current process -- they were
    // created by the parent process before it did fork + exec. However, since
    // exec causes the child to "forget" everything, we have to pass the numbers
    // of these file descriptors as arguments.
    //
    // I'm not entirely sure what this does on Windows.
    pub(crate) fn get_context() -> Option<(String, Context)> {
        let entry = std::env::var(ENV_DAEMON).ok().filter(|s| !s.is_empty())?;
        let file = std::env::var(ENV_DAEMON_PARAM)
            .ok()
            .filter(|s| !s.is_empty())?;
        let bytes =
            fs::read(&file).unwrap_or_else(|e| panic!("Can't find daemon parameters: {}", e));
        if let Err(e) = fs::remove_file(&file) {
            tracing::warn!(
                target: "flow_daemon",
                "Daemon::get_context: failed to remove param file {:?}: {}",
                file,
                e
            );
        }
        let (context, _): (Context, usize) =
            bincode::serde::decode_from_slice(&bytes, bincode::config::legacy())
                .unwrap_or_else(|e| panic!("Can't decode daemon parameters: {}", e));
        Some((entry, context))
    }

    // No-op in Rust: we never mutate the parent's env (we set per-child env
    // via `Command::env`), so there's nothing to clear. Kept as a function
    // for OCaml structural fidelity.
    pub(crate) fn clear_context() {}
}

pub use entry::Entry;

fn exec(entry: &str, in_sock: TcpStream, out_sock: TcpStream, param_bytes: Vec<u8>) -> ! {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        entry::find_and_call(entry, param_bytes, in_sock, out_sock);
    }));
    match result {
        Ok(()) => std::process::exit(0),
        Err(e) => {
            let msg: String = if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = e.downcast_ref::<&'static str>() {
                (*s).to_string()
            } else {
                "Daemon entry point panicked (non-string payload)".to_string()
            };
            eprintln!("{}", msg);
            std::process::exit(2)
        }
    }
}

pub fn register_entry_point<P, I, O>(
    name: &'static str,
    f: fn(P, ChannelPair<I, O>),
) -> Entry<P, I, O>
where
    P: Serialize + DeserializeOwned + Send + 'static,
    I: Serialize + DeserializeOwned + Send + 'static,
    O: Serialize + DeserializeOwned + Send + 'static,
{
    entry::register(name, f)
}

pub fn name_of_entry<P, I, O>(entry: &Entry<P, I, O>) -> &'static str {
    entry::name_of_entry(entry)
}

// `Daemon.fd_of_path` in OCaml returns a `Unix.file_descr`. We return a
// `std::fs::File` instead: it is the cross-platform Rust analogue (works on
// both Unix and Windows), is convertible to `Stdio` via `Stdio::from(File)`
// on every platform, and avoids the Unix-only `OwnedFd`.
pub fn fd_of_path(path: &Path) -> File {
    sys_utils::with_umask(0o111, || {
        if let Some(parent) = path.parent() {
            sys_utils::mkdir_no_fail(parent)
                .unwrap_or_else(|e| panic!("fd_of_path: mkdir_no_fail({:?}): {}", parent, e));
        }
        std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .unwrap_or_else(|e| panic!("fd_of_path: open({:?}): {}", path, e))
    })
}

pub fn null_fd() -> File {
    fd_of_path(Path::new(sys_utils::null_path()))
}

// `StdioFd` lets callers either inherit one of the parent's standard streams
// (matching OCaml's `if stdin <> Unix.stdin then close_if_open stdin` at lines
// 207-213, where passing `Unix.stdin` means "inherit") or pass an owned file
// that should be closed on the parent side after the spawn completes. We use
// `std::fs::File` rather than `OwnedFd` because `OwnedFd` is Unix-only;
// `File` is cross-platform and `Stdio::from(File)` exists on both Unix and
// Windows, so spawn behaves identically on every platform.
pub enum StdioFd {
    Inherit,
    Owned(File),
}

impl StdioFd {
    fn into_stdio(self) -> Stdio {
        match self {
            StdioFd::Inherit => Stdio::inherit(),
            StdioFd::Owned(file) => Stdio::from(file),
        }
    }
}

fn setup_channels(_channel_mode: ChannelMode) -> std::io::Result<(TcpListener, TcpListener)> {
    let parent_in_listener = TcpListener::bind("127.0.0.1:0")?;
    let parent_out_listener = TcpListener::bind("127.0.0.1:0")?;
    Ok((parent_in_listener, parent_out_listener))
}

fn accept_with_token(
    listener: &TcpListener,
    expected_token: &[u8; 32],
    timeout: Duration,
) -> std::io::Result<TcpStream> {
    // We must defend against a racing local process connecting to our
    // ephemeral port. Loop accepting until we see a connection bearing the
    // expected token; reject and close anything else. Bound by `timeout`.
    //
    // `TcpListener::accept` is unconditionally blocking. To honor the
    // deadline we put the listener into non-blocking mode and poll. Polling
    // sleeps 10ms between attempts -- a bounded busy-wait -- which is
    // negligible because the child is expected to connect within milliseconds
    // of exec.
    listener.set_nonblocking(true)?;
    let deadline = std::time::Instant::now() + timeout;
    loop {
        if std::time::Instant::now() >= deadline {
            return Err(std::io::Error::new(
                std::io::ErrorKind::TimedOut,
                "Daemon::spawn: timed out waiting for child to connect",
            ));
        }
        let (mut stream, _peer) = match listener.accept() {
            Ok(pair) => pair,
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                std::thread::sleep(Duration::from_millis(10));
                continue;
            }
            Err(e) => return Err(e),
        };
        // The accepted stream inherits the listener's nonblocking flag on
        // some platforms; ensure it is blocking and bounded by remaining time.
        stream.set_nonblocking(false)?;
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::TimedOut,
                "Daemon::spawn: timed out reading token from child",
            ));
        }
        stream.set_read_timeout(Some(remaining))?;
        let mut received = [0u8; 32];
        match stream.read_exact(&mut received) {
            Ok(()) if &received == expected_token => {
                stream.set_read_timeout(None)?;
                return Ok(stream);
            }
            Ok(()) => {
                tracing::warn!(
                    target: "flow_daemon",
                    "Daemon::spawn: rejected connection with bad token from {:?}",
                    stream.peer_addr().ok()
                );
                drop(stream);
            }
            Err(e) => {
                tracing::warn!(
                    target: "flow_daemon",
                    "Daemon::spawn: accept-with-token read error: {}",
                    e
                );
                drop(stream);
            }
        }
    }
}

pub fn spawn<P, I, O>(
    channel_mode: Option<ChannelMode>,
    name: Option<&str>,
    stdio: (StdioFd, StdioFd, StdioFd),
    entry: &Entry<P, I, O>,
    param: P,
) -> std::io::Result<Handle<O, I>>
where
    P: Serialize + DeserializeOwned + Send + 'static,
    I: Serialize + DeserializeOwned + Send + 'static,
    O: Serialize + DeserializeOwned + Send + 'static,
{
    let channel_mode = channel_mode.unwrap_or(ChannelMode::Pipe);
    let (parent_in_listener, parent_out_listener) = setup_channels(channel_mode)?;
    let parent_in_addr = parent_in_listener.local_addr()?;
    let parent_out_addr = parent_out_listener.local_addr()?;
    let token: [u8; 32] = rand::random();

    // Entry.set_context entry param (child_in, child_out);
    let param_file = entry::write_param_file(parent_in_addr, parent_out_addr, token, &param)?;

    // let exe = Sys_utils.executable_path () in
    let exe = sys_utils::executable_path();
    // let name = Base.Option.value ~default:(Entry.name_of_entry entry) name in
    let entry_name = name_of_entry(entry);
    let argv1 = name.unwrap_or(entry_name);

    // let pid = Unix.create_process exe [| exe; name |] stdin stdout stderr in
    let mut cmd = Command::new(exe);
    cmd.arg(argv1);
    cmd.env(entry::ENV_DAEMON, entry_name);
    cmd.env(entry::ENV_DAEMON_PARAM, &param_file);
    let (stdin_fd, stdout_fd, stderr_fd) = stdio;
    cmd.stdin(stdin_fd.into_stdio());
    cmd.stdout(stdout_fd.into_stdio());
    cmd.stderr(stderr_fd.into_stdio());

    let child = cmd.spawn()?;

    entry::clear_context();

    let parent_in_sock = accept_with_token(&parent_in_listener, &token, Duration::from_secs(30))?;
    let parent_out_sock = accept_with_token(&parent_out_listener, &token, Duration::from_secs(30))?;
    drop(parent_in_listener);
    drop(parent_out_listener);

    pid_log::log(Some(entry_name), /*no_fail=*/ true, child.id());

    Ok(Handle {
        channels: ChannelPair(
            InChannel {
                stream: parent_in_sock,
                _phantom: PhantomData,
            },
            OutChannel {
                stream: parent_out_sock,
                _phantom: PhantomData,
            },
        ),
        child,
    })
}

pub fn check_entry_point() {
    let Some((entry_name, context)) = entry::get_context() else {
        return;
    };
    entry::clear_context();
    let entry::Context {
        parent_in_addr,
        parent_out_addr,
        token,
        param_bytes,
    } = context;

    let mut child_in_sock = TcpStream::connect(parent_out_addr).unwrap_or_else(|e| {
        panic!(
            "Daemon child: failed to connect to parent out-socket (child read end): {}",
            e
        )
    });
    child_in_sock.write_all(&token).unwrap_or_else(|e| {
        panic!(
            "Daemon child: failed to write token to parent out-socket: {}",
            e
        )
    });
    let mut child_out_sock = TcpStream::connect(parent_in_addr).unwrap_or_else(|e| {
        panic!(
            "Daemon child: failed to connect to parent in-socket (child write end): {}",
            e
        )
    });
    child_out_sock.write_all(&token).unwrap_or_else(|e| {
        panic!(
            "Daemon child: failed to write token to parent in-socket: {}",
            e
        )
    });

    exec(&entry_name, child_in_sock, child_out_sock, param_bytes);
}

pub fn close<I, O>(h: &mut Handle<I, O>) -> std::io::Result<()> {
    h.channels.0.stream.shutdown(std::net::Shutdown::Read)?;
    h.channels.1.stream.shutdown(std::net::Shutdown::Write)?;
    Ok(())
}

pub fn close_noerr<I, O>(h: &mut Handle<I, O>) {
    if let Err(e) = h.channels.0.stream.shutdown(std::net::Shutdown::Read) {
        tracing::debug!(target: "flow_daemon", "close_noerr (in): {}", e);
    }
    if let Err(e) = h.channels.1.stream.shutdown(std::net::Shutdown::Write) {
        tracing::debug!(target: "flow_daemon", "close_noerr (out): {}", e);
    }
}

pub fn kill<I, O>(mut h: Handle<I, O>) -> std::io::Result<()> {
    close_noerr(&mut h);
    if h.child.id() == 0 {
        return Ok(());
    }
    match h.child.kill() {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::InvalidInput => {}
        Err(e) => return Err(e),
    }
    h.child.wait()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_entry_point_returns_when_env_unset() {
        check_entry_point();
    }
}
