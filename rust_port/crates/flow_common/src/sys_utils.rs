/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::env;
use std::io;
use std::path;
use std::path::Path;
use std::path::PathBuf;
use std::sync::OnceLock;

pub fn null_path() -> &'static str {
    #[cfg(windows)]
    {
        "nul"
    }
    #[cfg(not(windows))]
    {
        "/dev/null"
    }
}

pub fn temp_dir_name() -> PathBuf {
    env::temp_dir()
}

pub fn with_umask<R>(umask: u32, f: impl FnOnce() -> R) -> R {
    #[cfg(unix)]
    {
        let old = unix_umask::set(umask);
        struct Restore(u32);
        impl Drop for Restore {
            fn drop(&mut self) {
                unix_umask::set(self.0);
            }
        }
        let _restore = Restore(old);
        f()
    }
    #[cfg(not(unix))]
    {
        let _umask = umask;
        f()
    }
}

#[cfg(unix)]
mod unix_umask {
    use nix::sys::stat::Mode;
    use nix::sys::stat::umask;

    pub fn set(mask: u32) -> u32 {
        let new_mode = Mode::from_bits_truncate(mask as nix::libc::mode_t);
        let old_mode = umask(new_mode);
        old_mode.bits() as u32
    }
}

/// Like Python's os.path.expanduser, though probably doesn't cover some cases.
/// Roughly follow's bash's tilde expansion:
/// http://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html
///
/// ~/foo -> /home/bob/foo if $HOME = "/home/bob"
/// ~joe/foo -> /home/joe/foo if joe's home is /home/joe
pub fn expanduser(path: &str) -> String {
    if let Some(rest) = path.strip_prefix("~/") {
        if let Ok(home) = env::var("HOME") {
            return format!("{}/{}", home, rest);
        }
    }
    path.to_string()
}

pub fn executable_path() -> &'static Path {
    static CACHED: OnceLock<PathBuf> = OnceLock::new();
    CACHED
        .get_or_init(|| {
            env::current_exe()
                .unwrap_or_else(|e| panic!("Unable to determine executable path: {}", e))
        })
        .as_path()
}

pub fn mkdir_no_fail(dir: &Path) -> io::Result<()> {
    with_umask(0, || match std::fs::DirBuilder::new().create(dir) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(()),
        Err(e) => Err(e),
    })
}

pub fn is_rosetta() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| {
        #[cfg(target_os = "macos")]
        {
            match std::process::Command::new("sysctl")
                .args(["-n", "sysctl.proc_translated"])
                .output()
            {
                Ok(out) if out.status.success() => std::str::from_utf8(&out.stdout)
                    .map(|s| s.trim() == "1")
                    .unwrap_or(false),
                _ => false,
            }
        }
        #[cfg(not(target_os = "macos"))]
        {
            false
        }
    })
}

pub fn pid_of_handle(pid: u32) -> u32 {
    pid
}

/// Converts platform-specific directory separators to /
///
/// On Unix systems where the directory separator is already '/', this returns
/// the input without allocation (Cow::Borrowed).
/// On Windows, this converts backslashes to forward slashes (Cow::Owned).
pub fn normalize_filename_dir_sep(path: &str) -> Cow<'_, str> {
    let dir_sep = path::MAIN_SEPARATOR;

    if dir_sep == '/' {
        Cow::Borrowed(path)
    } else if path.contains(dir_sep) {
        Cow::Owned(
            path.chars()
                .map(|c| if c == dir_sep { '/' } else { c })
                .collect(),
        )
    } else {
        Cow::Borrowed(path)
    }
}
