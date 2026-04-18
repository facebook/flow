/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use md5::Digest;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Addr {
    Inet(std::net::IpAddr, u16),
    Unix(String),
}

// On Linux/Mac/BSD, sockaddr_un.sun_path is a fixed length. To handle longer paths,
// we chdir to that directory and use a relative path instead. The callback provides
// a Unix.sockaddr with a relative path that you can use to bind or read from. Perform
// as little as possible within the callback, since it has an unexpected working dir.
// This function tries to make it awkward for the Unix.sockaddr with the relative path
// to escape from the callback.
pub fn with_addr<F, R>(addr: &Addr, f: F) -> R
where
    F: FnOnce(&str) -> R,
{
    static CHDIR_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let _guard = CHDIR_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    match addr {
        Addr::Inet(_, _) => {
            panic!("Socket.with_addr: Inet addr not supported in Rust port");
        }
        Addr::Unix(file) => {
            let cwd = std::env::current_dir().expect("getcwd failed");
            let dir = std::path::Path::new(file)
                .parent()
                .unwrap_or_else(|| std::path::Path::new("."));
            let base = std::path::Path::new(file)
                .file_name()
                .expect("Socket.with_addr: file has no basename");
            std::env::set_current_dir(dir).expect("chdir to socket dir failed");
            let relative = format!("./{}", base.to_string_lossy());
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&relative)));
            let _ = std::env::set_current_dir(&cwd);
            match result {
                Ok(r) => r,
                Err(payload) => std::panic::resume_unwind(payload),
            }
        }
    }
}

// The sockaddr_un structure puts a strict limit on the length of a socket
// address. This appears to be 104 chars on mac os x and 108 chars on my
// centos box. Since `with_addr` uses a relative path, `get_path` shortens
// the basename to fit if necessary.
const MAX_ADDR_LENGTH: usize = 103;

pub fn get_path(path: &str) -> String {
    // Path will resolve the realpath, in case two processes are referring to the
    // same socket using different paths (like with symlinks).
    let path_buf = std::path::PathBuf::from(path);
    let canonical = match (path_buf.parent(), path_buf.file_name()) {
        (Some(parent), Some(base)) => match std::fs::canonicalize(parent) {
            Ok(canon_parent) => canon_parent.join(base),
            Err(_) => path_buf,
        },
        _ => path_buf,
    };
    let path = canonical.to_string_lossy().into_owned();
    let dir = std::path::Path::new(&path)
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|| ".".to_string());
    let filename = std::path::Path::new(&path)
        .file_name()
        .map(|f| f.to_string_lossy().into_owned())
        .unwrap_or_default();
    let (root_part, extension): (String, String) = match filename.rfind('.') {
        Some(idx) => (filename[..idx].to_string(), filename[idx..].to_string()),
        None => (filename.clone(), String::new()),
    };
    let root_length = root_part.len();
    let extension_length = extension.len();
    let dir_sep_length = 1;
    let max_root_part_length = MAX_ADDR_LENGTH
        .saturating_sub(dir_sep_length)
        .saturating_sub(extension_length)
        .saturating_sub(1);
    let root_part = if root_length > max_root_part_length {
        let prefix = &root_part[..5];
        let suffix = &root_part[root_length - 5..];
        let digest = format!("{:x}", md5::Md5::digest(root_part.as_bytes()));
        // 5 char prefix + 5 char suffix + 2 periods
        let max_digest_length = max_root_part_length.saturating_sub(12);
        let digest_part = if max_digest_length == 0 {
            eprintln!("Socket name is too long: {:?}", filename);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
        } else if digest.len() > max_digest_length {
            digest[..max_digest_length].to_string()
        } else {
            digest
        };
        format!("{}.{}.{}", prefix, digest_part, suffix)
    } else {
        root_part
    };
    format!("{}/{}{}", dir, root_part, extension)
}

pub fn addr_for_open(sockfile: &str) -> Addr {
    let sock_name = get_path(sockfile);
    Addr::Unix(sock_name)
}
