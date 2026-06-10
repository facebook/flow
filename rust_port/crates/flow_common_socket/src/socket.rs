/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
#[cfg(windows)]
use std::sync::Arc;
#[cfg(windows)]
use std::sync::Mutex;
use std::time::Duration;

use md5::Digest;
#[cfg(unix)]
use socket2::Domain;
#[cfg(unix)]
use socket2::SockAddr;
#[cfg(unix)]
use socket2::Socket;
#[cfg(unix)]
use socket2::Type;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Addr {
    #[cfg(windows)]
    NamedPipe(String),
    #[cfg(unix)]
    Unix(String),
}

// On Linux/Mac/BSD, sockaddr_un.sun_path is a fixed length. To handle longer paths,
// we chdir to that directory and use a relative path instead. The callback provides
// a Unix.sockaddr with a relative path that you can use to bind or read from. Perform
// as little as possible within the callback, since it has an unexpected working dir.
// This function tries to make it awkward for the Unix.sockaddr with the relative path
// to escape from the callback.
#[cfg(unix)]
pub fn with_addr<T>(addr: &Addr, f: impl FnOnce(&SockAddr) -> io::Result<T>) -> io::Result<T> {
    let cwd = std::env::current_dir()?;
    match addr {
        Addr::Unix(file) => {
            let path = Path::new(file);
            let dir = path.parent().unwrap_or_else(|| Path::new("."));
            let base = path.file_name().ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "socket path has no basename")
            })?;
            std::env::set_current_dir(dir)?;
            let mut guard = CwdGuard(Some(cwd));
            let sockaddr = SockAddr::unix(Path::new(".").join(base))?;
            let result = f(&sockaddr);
            guard.restore()?;
            result
        }
    }
}

// Initializes the unix domain socket
fn unix_socket(sock_name: &str) -> io::Result<SocketListener> {
    flow_common::sys_utils::with_umask(0o111, || {
        let dir = Path::new(sock_name)
            .parent()
            .filter(|dir| !dir.as_os_str().is_empty())
            .unwrap_or_else(|| Path::new("."));
        flow_common::sys_utils::mkdir_no_fail(dir)?;
        if Path::new(sock_name).exists() {
            std::fs::remove_file(sock_name)?;
        }
        #[cfg(windows)]
        {
            let addr = Addr::NamedPipe(pipe_name(sock_name));
            let Addr::NamedPipe(pipe_name) = addr;
            let sock = create_pipe_instance(&pipe_name, true)?;
            std::fs::write(sock_name, &pipe_name)?;
            Ok(SocketListener {
                pipe_name,
                pending: Some(sock),
            })
        }
        #[cfg(unix)]
        {
            let (domain, addr) = (Domain::UNIX, Addr::Unix(sock_name.to_string()));
            let sock = Socket::new(domain, Type::STREAM, None)?;
            sock.set_reuse_address(true)?;
            with_addr(&addr, |addr| sock.bind(addr))?;
            sock.listen(10)?;
            Ok(SocketListener { socket: sock })
        }
    })
}

// The sockaddr_un structure puts a strict limit on the length of a socket
// address. This appears to be 104 chars on mac os x and 108 chars on my
// centos box. Since `with_addr` uses a relative path, `get_path` shortens
// the basename to fit if necessary.
const MAX_ADDR_LENGTH: isize = 103;

pub fn get_path(path: &str) -> String {
    // Path will resolve the realpath, in case two processes are referring to the
    // same socket using different paths (like with symlinks *)
    #[cfg(windows)]
    let path_buf = PathBuf::from(path.replace('/', "\\"));
    #[cfg(not(windows))]
    let path_buf = PathBuf::from(path);
    let canonical = match std::fs::canonicalize(&path_buf) {
        Ok(path) => path,
        Err(_) => path_buf,
    };
    let path = canonical.to_string_lossy().into_owned();
    let dir = Path::new(&path)
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|| ".".to_string());
    let filename = Path::new(&path)
        .file_name()
        .map(|f| f.to_string_lossy().into_owned())
        .unwrap_or_default();
    let (root_part, extension): (String, String) = match filename.rfind('.') {
        Some(idx) => (filename[..idx].to_string(), filename[idx..].to_string()),
        None => (filename.clone(), String::new()),
    };
    let root_length = root_part.len();
    let extension_length = extension.len();
    let dir_sep_length = std::path::MAIN_SEPARATOR.to_string().len();
    let max_root_part_length =
        MAX_ADDR_LENGTH - dir_sep_length as isize - extension_length as isize - 1;
    let root_part = if root_length as isize > max_root_part_length {
        let len = root_part.len();
        let prefix = &root_part[..5];
        let suffix = &root_part[len - 5..];
        let digest = format!("{:x}", md5::Md5::digest(root_part.as_bytes()));
        let max_digest_length = max_root_part_length - 12;
        let digest_part = if max_digest_length <= 0 {
            eprintln!("Socket name is too long: {:?}", filename);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
        } else if digest.len() > max_digest_length as usize {
            digest[..max_digest_length as usize].to_string()
        } else {
            digest
        };
        format!("{}.{}.{}", prefix, digest_part, suffix)
    } else {
        root_part
    };
    Path::new(&dir)
        .join(format!("{}{}", root_part, extension))
        .to_string_lossy()
        .into_owned()
}

pub fn addr_for_open(sockfile: &str) -> io::Result<Addr> {
    let sock_name = get_path(sockfile);
    #[cfg(windows)]
    {
        let pipe_name = std::fs::read_to_string(sock_name)?;
        Ok(Addr::NamedPipe(pipe_name))
    }
    #[cfg(unix)]
    {
        Ok(Addr::Unix(sock_name))
    }
}

pub fn init_unix_socket(socket_file: &str) -> SocketListener {
    let sock_name = get_path(socket_file);
    match unix_socket(&sock_name) {
        Ok(listener) => listener,
        Err(e) => {
            eprintln!("{}", e);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
        }
    }
}

pub struct SocketListener {
    #[cfg(unix)]
    socket: Socket,
    #[cfg(windows)]
    pipe_name: String,
    #[cfg(windows)]
    pending: Option<tokio::net::windows::named_pipe::NamedPipeServer>,
}

pub struct SocketStream {
    #[cfg(unix)]
    socket: Socket,
    #[cfg(windows)]
    pipe: Arc<NamedPipeStream>,
    #[cfg(windows)]
    read_timeout: Arc<Mutex<Option<Duration>>>,
    #[cfg(windows)]
    write_timeout: Arc<Mutex<Option<Duration>>>,
}

#[cfg(windows)]
enum NamedPipeStream {
    Client(tokio::net::windows::named_pipe::NamedPipeClient),
    Server(tokio::net::windows::named_pipe::NamedPipeServer),
}

#[cfg(unix)]
struct CwdGuard(Option<PathBuf>);

#[cfg(unix)]
impl CwdGuard {
    fn restore(&mut self) -> io::Result<()> {
        match self.0.take() {
            Some(cwd) => std::env::set_current_dir(cwd),
            None => Ok(()),
        }
    }
}

#[cfg(unix)]
impl Drop for CwdGuard {
    fn drop(&mut self) {
        if let Some(cwd) = self.0.take() {
            match std::env::set_current_dir(cwd) {
                Ok(()) | Err(_) => {}
            }
        }
    }
}

#[cfg(windows)]
fn pipe_name(sock_name: &str) -> String {
    let digest = format!("{:x}", md5::Md5::digest(sock_name.as_bytes()));
    format!(r"\\.\pipe\flow-{}", digest)
}

#[cfg(windows)]
fn create_pipe_instance(
    pipe_name: &str,
    first_pipe_instance: bool,
) -> io::Result<tokio::net::windows::named_pipe::NamedPipeServer> {
    flow_tokio_runtime::block_on(async {
        let mut options = tokio::net::windows::named_pipe::ServerOptions::new();
        options.reject_remote_clients(true);
        options.first_pipe_instance(first_pipe_instance);
        options.create(pipe_name)
    })
}

impl SocketListener {
    pub fn accept(&mut self) -> io::Result<SocketStream> {
        #[cfg(unix)]
        {
            let (socket, _addr) = self.socket.accept()?;
            Ok(SocketStream { socket })
        }
        #[cfg(windows)]
        {
            let server = match self.pending.take() {
                Some(server) => server,
                None => create_pipe_instance(&self.pipe_name, false)?,
            };
            let connect_result = flow_tokio_runtime::block_on(server.connect());
            match connect_result {
                Ok(()) => {
                    self.pending = Some(create_pipe_instance(&self.pipe_name, false)?);
                    Ok(SocketStream {
                        pipe: Arc::new(NamedPipeStream::Server(server)),
                        read_timeout: Arc::new(Mutex::new(None)),
                        write_timeout: Arc::new(Mutex::new(None)),
                    })
                }
                Err(e) => {
                    self.pending = Some(create_pipe_instance(&self.pipe_name, false)?);
                    Err(e)
                }
            }
        }
    }
}

impl SocketStream {
    pub fn connect(addr: &Addr, timeout: Duration) -> io::Result<Self> {
        #[cfg(unix)]
        {
            let socket = Socket::new(Domain::UNIX, Type::STREAM, None)?;
            with_addr(addr, |addr| socket.connect_timeout(addr, timeout))?;
            Ok(Self { socket })
        }
        #[cfg(windows)]
        {
            const ERROR_PIPE_BUSY: i32 = 231;
            let deadline = std::time::Instant::now() + timeout;
            let pipe_name = match addr {
                Addr::NamedPipe(pipe_name) => pipe_name,
            };
            loop {
                let result = flow_tokio_runtime::block_on(async {
                    tokio::net::windows::named_pipe::ClientOptions::new().open(pipe_name)
                });
                match result {
                    Ok(client) => {
                        return Ok(Self {
                            pipe: Arc::new(NamedPipeStream::Client(client)),
                            read_timeout: Arc::new(Mutex::new(None)),
                            write_timeout: Arc::new(Mutex::new(None)),
                        });
                    }
                    Err(e)
                        if e.kind() == io::ErrorKind::NotFound
                            || e.raw_os_error() == Some(ERROR_PIPE_BUSY) =>
                    {
                        if std::time::Instant::now() >= deadline {
                            return Err(io::Error::new(
                                io::ErrorKind::TimedOut,
                                "timed out connecting to named pipe",
                            ));
                        }
                        std::thread::sleep(Duration::from_millis(10));
                    }
                    Err(e) => return Err(e),
                }
            }
        }
    }

    pub fn try_clone(&self) -> io::Result<Self> {
        #[cfg(unix)]
        {
            Ok(Self {
                socket: self.socket.try_clone()?,
            })
        }
        #[cfg(windows)]
        {
            Ok(Self {
                pipe: self.pipe.clone(),
                read_timeout: Arc::new(Mutex::new(*self.read_timeout.lock().unwrap())),
                write_timeout: Arc::new(Mutex::new(*self.write_timeout.lock().unwrap())),
            })
        }
    }

    pub fn set_read_timeout(&self, timeout: Option<Duration>) -> io::Result<()> {
        #[cfg(unix)]
        {
            self.socket.set_read_timeout(timeout)
        }
        #[cfg(windows)]
        {
            *self.read_timeout.lock().unwrap() = timeout;
            Ok(())
        }
    }

    pub fn set_write_timeout(&self, timeout: Option<Duration>) -> io::Result<()> {
        #[cfg(unix)]
        {
            self.socket.set_write_timeout(timeout)
        }
        #[cfg(windows)]
        {
            *self.write_timeout.lock().unwrap() = timeout;
            Ok(())
        }
    }

    pub fn shutdown(&self) -> io::Result<()> {
        #[cfg(unix)]
        {
            self.socket.shutdown(std::net::Shutdown::Both)
        }
        #[cfg(windows)]
        {
            match &*self.pipe {
                NamedPipeStream::Client(_) => Ok(()),
                NamedPipeStream::Server(server) => server.disconnect(),
            }
        }
    }

    pub fn set_nodelay(&self, _nodelay: bool) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(windows)]
fn wait_for_ready(
    timeout: Option<Duration>,
    ready: impl std::future::Future<Output = io::Result<()>>,
) -> io::Result<()> {
    match timeout {
        None => flow_tokio_runtime::block_on(ready),
        Some(timeout) => {
            // `tokio::time::timeout` constructs a `Sleep` that registers with
            // the reactor at construction time, so it must be created inside
            // the runtime context, not before `block_on`.
            match flow_tokio_runtime::block_on(
                async move { tokio::time::timeout(timeout, ready).await },
            ) {
                Ok(result) => result,
                Err(_) => Err(io::Error::new(io::ErrorKind::TimedOut, "socket timeout")),
            }
        }
    }
}

impl Read for SocketStream {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        #[cfg(unix)]
        {
            self.socket.read(buf)
        }
        #[cfg(windows)]
        {
            loop {
                let result = match &*self.pipe {
                    NamedPipeStream::Client(client) => client.try_read(buf),
                    NamedPipeStream::Server(server) => server.try_read(buf),
                };
                match result {
                    Ok(n) => return Ok(n),
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                        let timeout = *self.read_timeout.lock().unwrap();
                        match &*self.pipe {
                            NamedPipeStream::Client(client) => {
                                wait_for_ready(timeout, client.readable())?;
                            }
                            NamedPipeStream::Server(server) => {
                                wait_for_ready(timeout, server.readable())?;
                            }
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
        }
    }
}

impl Write for SocketStream {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        #[cfg(unix)]
        {
            self.socket.write(buf)
        }
        #[cfg(windows)]
        {
            loop {
                let result = match &*self.pipe {
                    NamedPipeStream::Client(client) => client.try_write(buf),
                    NamedPipeStream::Server(server) => server.try_write(buf),
                };
                match result {
                    Ok(n) => return Ok(n),
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                        let timeout = *self.write_timeout.lock().unwrap();
                        match &*self.pipe {
                            NamedPipeStream::Client(client) => {
                                wait_for_ready(timeout, client.writable())?;
                            }
                            NamedPipeStream::Server(server) => {
                                wait_for_ready(timeout, server.writable())?;
                            }
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
