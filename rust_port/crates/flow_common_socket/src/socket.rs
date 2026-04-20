/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The Rust port unifies on TCP-on-loopback for all platforms, for cross-platform consistency.
// The on-disk port-file format is the decimal port number as text
// (matching `flow_server::standalone`'s existing convention), so any
// client can use `std::fs::read_to_string` + `parse::<u16>` to recover the port.

use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::net::TcpListener;

use md5::Digest;

// The sockaddr_un structure puts a strict limit on the length of a socket
// address. This appears to be 104 chars on mac os x and 108 chars on my
// centos box. With the TCP unification, the path is used to store a port
// number on disk; the same shortening rule still applies because the port
// file path goes through the same temp-dir hierarchy.
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

// Initializes a TCP listener on 127.0.0.1:0, writes the assigned port into `sock_name`
// in decimal-text format (matching `flow_server::standalone`'s existing convention),
// and returns the `TcpListener`. Mirrors OCaml `unix_socket` for the `Sys.win32` branch.
fn tcp_socket(sock_name: &str) -> TcpListener {
    if let Some(parent) = std::path::Path::new(sock_name).parent() {
        if !parent.as_os_str().is_empty() {
            match std::fs::create_dir_all(parent) {
                Ok(()) => {}
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
                Err(e) => {
                    eprintln!("{}", e);
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::SocketError,
                    );
                }
            }
        }
    }
    if std::path::Path::new(sock_name).exists() {
        match std::fs::remove_file(sock_name) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                eprintln!("{}", e);
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
            }
        }
    }
    let bind_addr = SocketAddr::from((Ipv4Addr::LOCALHOST, 0));
    let listener = match TcpListener::bind(bind_addr) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("{}", e);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
        }
    };
    let port = match listener.local_addr() {
        Ok(addr) => addr.port(),
        Err(e) => {
            eprintln!("{}", e);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
        }
    };
    if let Err(e) = std::fs::write(sock_name, port.to_string()) {
        eprintln!("{}", e);
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::SocketError);
    }
    listener
}

pub fn init_tcp_socket(socket_file: &str) -> TcpListener {
    tcp_socket(&get_path(socket_file))
}
