/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module makes Lwt's logger easily available for use. Lwt's logger is nice because it
// can easily log exceptions, format messages, and interact with Lwt.
//
// Initially I used Lwt_io for this module's implementation. It was nice, because Lwt_io.atomic
// guaranteed that I wouldn't have multiple threads interleaving their logs. However, I found that
// Lwt_io.flush_all which is called by at_exit could take awhile. Furthermore, I was able to
// trigger deadlocks by logging & flushing around exit time.
//
// So now the implementation uses fds exclusively.
//
// 1. Multiple threads write to the msg_stream with their logs
// 2. A single thread (WriteLoop) reads the messages and writes them to the various fds directly

use std::fs::File;
use std::io::Write;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::atomic::AtomicU8;
use std::sync::atomic::Ordering;

use crate::runtime;

#[derive(Clone, Copy)]
struct Dest {
    file: bool,
    stderr: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum Level {
    Off = 0,
    Fatal = 1,
    Error = 2,
    Warn = 3,
    Info = 4,
    Debug = 5,
}

impl Level {
    fn from_u8(v: u8) -> Self {
        match v {
            0 => Level::Off,
            1 => Level::Fatal,
            2 => Level::Error,
            3 => Level::Warn,
            4 => Level::Info,
            _ => Level::Debug,
        }
    }
}

static MIN_LEVEL: AtomicU8 = AtomicU8::new(Level::Info as u8);
static MIN_LEVEL_FILE: AtomicU8 = AtomicU8::new(Level::Info as u8);
static MIN_LEVEL_STDERR: AtomicU8 = AtomicU8::new(Level::Info as u8);

pub fn set_min_level(level: Level) {
    MIN_LEVEL.store(level as u8, Ordering::Release);
}

pub fn set_min_level_file(level: Level) {
    MIN_LEVEL_FILE.store(level as u8, Ordering::Release);
}

pub fn set_min_level_stderr(level: Level) {
    MIN_LEVEL_STDERR.store(level as u8, Ordering::Release);
}

pub fn min_level() -> Level {
    Level::from_u8(MIN_LEVEL.load(Ordering::Acquire))
}

pub fn min_level_file() -> Level {
    Level::from_u8(MIN_LEVEL_FILE.load(Ordering::Acquire))
}

pub fn min_level_stderr() -> Level {
    Level::from_u8(MIN_LEVEL_STDERR.load(Ordering::Acquire))
}

// We're using lwt's logger instead of Hh_logger, so let's map Hh_logger levels to lwt levels
type MsgPayload = (Dest, Vec<String>);

static MSG_SENDER: OnceLock<tokio::sync::mpsc::UnboundedSender<MsgPayload>> = OnceLock::new();

fn push_to_msg_stream(payload: MsgPayload) {
    if let Some(tx) = MSG_SENDER.get() {
        if let Err(send_err) = tx.send(payload) {
            eprintln!(
                "flow_server_monitor_logger: dropped log message ({} bytes of formatted text); WriteLoop receiver closed",
                send_err.0.1.iter().map(|s| s.len()).sum::<usize>()
            );
        }
    }
}

mod write_loop {
    use super::*;

    // Given a list of messages and a fd, write them serially to the fd
    pub(super) fn write_msgs(msgs: &[String], fd: &mut dyn Write) {
        for msg in msgs {
            if let Err(io_err) = fd.write_all(msg.as_bytes()) {
                eprintln!("Logger.WriteLoop write_msgs failure: {}", io_err);
                std::process::abort();
            }
        }
    }

    // Get a list of messages, write the list in parallel to each fd
    pub(super) async fn main(
        file_fd: &Option<Mutex<File>>,
        rx: &mut tokio::sync::mpsc::UnboundedReceiver<MsgPayload>,
    ) -> Option<()> {
        let (dest, msgs) = rx.recv().await?;
        match (dest, file_fd) {
            (
                Dest {
                    stderr: true,
                    file: true,
                },
                Some(file),
            ) => {
                write_msgs(&msgs, &mut std::io::stderr());
                write_msgs(&msgs, &mut *file.lock().unwrap());
            }
            (
                Dest {
                    stderr: false,
                    file: true,
                },
                Some(file),
            ) => {
                write_msgs(&msgs, &mut *file.lock().unwrap());
            }
            (Dest { stderr: true, .. }, None) => {
                write_msgs(&msgs, &mut std::io::stderr());
            }
            _ => {}
        }
        Some(())
    }

    // If we failed to write to an fd throw an exception and exit. I'm not 100% sure this is the
    // best behavior - should logging errors cause the monitor (and server) to crash?
    pub(super) fn catch(exn: &dyn std::fmt::Display) {
        eprintln!("Logger.WriteLoop exception:\n{}", exn);
        std::process::abort();
    }

    pub(super) async fn run(
        file_fd: Option<Mutex<File>>,
        mut rx: tokio::sync::mpsc::UnboundedReceiver<MsgPayload>,
    ) {
        loop {
            match main(&file_fd, &mut rx).await {
                Some(()) => continue,
                None => return,
            }
        }
    }

    #[allow(dead_code)]
    pub(super) fn report_panic(payload: &dyn std::fmt::Display) {
        catch(payload);
    }
}

static INITIALIZED: OnceLock<()> = OnceLock::new();

struct MonitorLogger;

fn timestamp_string() -> String {
    let now = chrono::Local::now();
    now.format("[%Y-%m-%d %H:%M:%S%.3f]").to_string()
}

fn string_of_level(level: Level) -> &'static str {
    match level {
        Level::Off => "off",
        Level::Fatal => "fatal",
        Level::Error => "error",
        Level::Warn => "warning",
        Level::Info => "info",
        Level::Debug => "debug",
    }
}

fn level_of_log_level(l: log::Level) -> Level {
    match l {
        log::Level::Error => Level::Error,
        log::Level::Warn => Level::Warn,
        log::Level::Info => Level::Info,
        log::Level::Debug => Level::Debug,
        log::Level::Trace => Level::Debug,
    }
}

// Format the messages and write the to the log and stderr
fn output(level: Level, messages: Vec<String>) {
    let dest = Dest {
        file: level <= min_level_file(),
        stderr: level <= min_level_stderr(),
    };
    let level_str = string_of_level(level);
    let formatted_messages: Vec<String> = messages
        .into_iter()
        .map(|message| {
            format!(
                "{} [monitor][{}] {}\n",
                timestamp_string(),
                level_str,
                message,
            )
        })
        .collect();
    push_to_msg_stream((dest, formatted_messages));
}

impl log::Log for MonitorLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        let level = level_of_log_level(metadata.level());
        level <= min_level()
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }
        let level = level_of_log_level(record.level());
        output(level, vec![format!("{}", record.args())]);
    }

    fn flush(&self) {
        if let Err(io_err) = std::io::stderr().flush() {
            let diag = format!(
                "flow_server_monitor_logger: stderr flush failed: {}\n",
                io_err
            );
            use std::os::fd::AsFd;
            let stderr_handle = std::io::stderr();
            if let Ok(()) =
                nix::unistd::write(stderr_handle.as_fd(), diag.as_bytes()).map(|_n_written| ())
            {
            }
        }
    }
}

// Creates a default logger and sets the minimum logger level. The logger will log every message
// that passes the minimum level to stderr. If log_fd is provided, each message will be logged
// to it as well
pub fn init_logger(log_file: Option<File>) {
    if INITIALIZED.set(()).is_err() {
        panic!("Cannot initialized FlowServerMonitorLogger more than once");
    }

    let min_level = min_level();

    let file = log_file.map(Mutex::new);

    let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<MsgPayload>();
    if MSG_SENDER.set(tx).is_err() {
        panic!("logger msg_sender already initialized");
    }
    runtime::handle().spawn(async move {
        let panic_outcome = std::panic::AssertUnwindSafe(write_loop::run(file, rx));
        if let Err(panic_payload) = futures::FutureExt::catch_unwind(panic_outcome).await {
            let msg: String = match panic_payload.downcast_ref::<&'static str>() {
                Some(s) => (*s).to_string(),
                None => match panic_payload.downcast_ref::<String>() {
                    Some(s) => s.clone(),
                    None => "WriteLoop panicked".to_string(),
                },
            };
            write_loop::report_panic(&msg);
        }
    });

    static LOGGER: MonitorLogger = MonitorLogger;
    if let Err(set_err) = log::set_logger(&LOGGER) {
        eprintln!(
            "flow_server_monitor_logger: log::set_logger failed (process logger already installed): {}",
            set_err
        );
    }

    log::set_max_level(match min_level {
        Level::Off => log::LevelFilter::Off,
        Level::Fatal => log::LevelFilter::Error,
        Level::Error => log::LevelFilter::Error,
        Level::Warn => log::LevelFilter::Warn,
        Level::Info => log::LevelFilter::Info,
        Level::Debug => log::LevelFilter::Debug,
    });
}

// Async logging APIs. These are the APIs you should generally use. Since they're async, they
// won't make the monitor unresponsive while they're logging
pub fn fatal(exn: Option<&str>, msg: &str) {
    output_with_exn(Level::Fatal, exn, msg);
}

pub fn error(exn: Option<&str>, msg: &str) {
    output_with_exn(Level::Error, exn, msg);
}

pub fn warn(exn: Option<&str>, msg: &str) {
    output_with_exn(Level::Warn, exn, msg);
}

pub fn info(exn: Option<&str>, msg: &str) {
    output_with_exn(Level::Info, exn, msg);
}

pub fn debug(exn: Option<&str>, msg: &str) {
    output_with_exn(Level::Debug, exn, msg);
}

fn output_with_exn(level: Level, exn: Option<&str>, msg: &str) {
    if level > min_level() {
        return;
    }
    let formatted = match exn {
        Some(e) => format!("{}\n{}", msg, e),
        None => msg.to_string(),
    };
    output(level, vec![formatted]);
}

// Synchronous versions just delegate to Hh_logger. These are mainly used for debugging, when you
// want a logging call to write to the log RIGHT NOW.
pub fn fatal_s(msg: &str) {
    log::error!("{}", msg);
}

pub fn error_s(msg: &str) {
    log::error!("{}", msg);
}

pub fn warn_s(msg: &str) {
    log::warn!("{}", msg);
}

pub fn info_s(msg: &str) {
    log::info!("{}", msg);
}

pub fn debug_s(msg: &str) {
    log::debug!("{}", msg);
}
