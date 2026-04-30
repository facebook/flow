/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs::File;
use std::io::Write;
use std::sync::Mutex;

pub fn timestamp_string() -> String {
    let now = chrono::Local::now();
    now.format("[%Y-%m-%d %H:%M:%S%.3f]").to_string()
}

#[derive(Clone, Copy)]
struct Dest {
    file: bool,
    stderr: bool,
}

// We might want to log to both stderr and a file. Shelling out to tee isn't cross-platform.
// We could dup2 stderr to a pipe and have a child process write to both original stderr and the
// file, but that's kind of overkill. This is good enough
static DUPE_LOG: Mutex<Option<(String, File)>> = Mutex::new(None);

pub fn set_log(filename: String, fd: File) {
    *DUPE_LOG.lock().unwrap() = Some((filename, fd));
}

pub fn get_log_name() -> Option<String> {
    DUPE_LOG
        .lock()
        .unwrap()
        .as_ref()
        .map(|(name, _)| name.clone())
}

static ID: Mutex<Option<String>> = Mutex::new(None);

pub fn set_id(passed_id: String) {
    *ID.lock().unwrap() = Some(passed_id);
}

fn id_string() -> String {
    match &*ID.lock().unwrap() {
        None => String::new(),
        Some(id) => format!("[{}] ", id),
    }
}

fn print_with_newline_internal(dest: Dest, s: &str) {
    let time = timestamp_string();
    let id_str = id_string();
    {
        let mut dupe_log = DUPE_LOG.lock().unwrap();
        match (&mut *dupe_log, dest.file) {
            (Some((_, dupe_log_oc)), true) => {
                let line = format!("{} {}{}\n", time, id_str, s);
                dupe_log_oc
                    .write_all(line.as_bytes())
                    .expect("Hh_logger: write to dupe log file failed");
                dupe_log_oc
                    .flush()
                    .expect("Hh_logger: flush of dupe log file failed");
            }
            (_, _) => (),
        }
    };
    if dest.stderr {
        let mut stderr = std::io::stderr().lock();
        let line = format!("{} {}{}\n", time, id_str, s);
        stderr
            .write_all(line.as_bytes())
            .expect("Hh_logger: write to stderr failed");
        stderr.flush().expect("Hh_logger: flush of stderr failed");
    }
}

pub fn print_with_newline(msg: String) {
    print_with_newline_internal(
        Dest {
            file: true,
            stderr: true,
        },
        &msg,
    );
}

pub fn print_duration(name: &str, t: f64) -> f64 {
    let t2 = chrono::Utc::now().timestamp_micros() as f64 / 1_000_000.0;
    print_with_newline(format!("{}: {:.6}", name, t2 - t));
    t2
}

pub mod level {
    use std::sync::Mutex;

    use super::Dest;
    use super::print_with_newline_internal;

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
    pub enum T {
        Off,
        Fatal,
        Error,
        Warn,
        Info,
        Debug,
    }

    fn int_of_level(l: T) -> u8 {
        match l {
            T::Off => 6,
            T::Fatal => 5,
            T::Error => 4,
            T::Warn => 3,
            T::Info => 2,
            T::Debug => 1,
        }
    }

    static MIN_LEVEL_FILE_REF: Mutex<T> = Mutex::new(T::Info);

    static MIN_LEVEL_STDERR_REF: Mutex<T> = Mutex::new(T::Info);

    pub fn min_level_file() -> T {
        *MIN_LEVEL_FILE_REF.lock().unwrap()
    }

    pub fn min_level_stderr() -> T {
        *MIN_LEVEL_STDERR_REF.lock().unwrap()
    }

    pub fn min_level() -> T {
        let file_int = int_of_level(*MIN_LEVEL_FILE_REF.lock().unwrap());
        let stderr_int = int_of_level(*MIN_LEVEL_STDERR_REF.lock().unwrap());
        if file_int < stderr_int {
            *MIN_LEVEL_FILE_REF.lock().unwrap()
        } else {
            *MIN_LEVEL_STDERR_REF.lock().unwrap()
        }
    }

    pub fn set_min_level_file(level: T) {
        *MIN_LEVEL_FILE_REF.lock().unwrap() = level;
    }

    pub fn set_min_level_stderr(level: T) {
        *MIN_LEVEL_STDERR_REF.lock().unwrap() = level;
    }

    pub fn set_min_level(level: T) {
        set_min_level_file(level);
        set_min_level_stderr(level);
    }

    fn passes_min_level_file(level: T) -> bool {
        int_of_level(level) >= int_of_level(*MIN_LEVEL_FILE_REF.lock().unwrap())
    }

    fn passes_min_level_stderr(level: T) -> bool {
        int_of_level(level) >= int_of_level(*MIN_LEVEL_STDERR_REF.lock().unwrap())
    }

    pub fn passes_min_level(level: T) -> bool {
        passes_min_level_file(level) || passes_min_level_stderr(level)
    }

    pub fn log(level: T, msg: String) {
        let dest = Dest {
            file: passes_min_level_file(level),
            stderr: passes_min_level_stderr(level),
        };
        if dest.file || dest.stderr {
            print_with_newline_internal(dest, &msg);
        }
    }

    //   let log_duration level fmt t =
    //     if passes_min_level level then
    //       print_duration fmt t
    //     else
    //       t
    pub fn log_duration(level: T, fmt: &str, t: f64) -> f64 {
        if passes_min_level(level) {
            super::print_duration(fmt, t)
        } else {
            t
        }
    }
}

pub use level::T as Level;

// Default log instructions to INFO level
#[macro_export]
macro_rules! log {
    (lvl = $lvl:expr, $($arg:tt)*) => {
        $crate::level::log($lvl, ::std::format!($($arg)*))
    };
    ($($arg:tt)*) => {
        $crate::level::log($crate::Level::Info, ::std::format!($($arg)*))
    };
}

pub fn log_duration(fmt: &str, t: f64) -> f64 {
    level::log_duration(Level::Info, fmt, t)
}

#[macro_export]
macro_rules! fatal {
    ($($arg:tt)*) => {
        $crate::level::log($crate::Level::Fatal, ::std::format!($($arg)*))
    };
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        $crate::level::log($crate::Level::Error, ::std::format!($($arg)*))
    };
}

#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {
        $crate::level::log($crate::Level::Warn, ::std::format!($($arg)*))
    };
}

#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        $crate::level::log($crate::Level::Info, ::std::format!($($arg)*))
    };
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        $crate::level::log($crate::Level::Debug, ::std::format!($($arg)*))
    };
}

#[macro_export]
macro_rules! print_with_newline {
    ($($arg:tt)*) => {
        $crate::print_with_newline(::std::format!($($arg)*))
    };
}
