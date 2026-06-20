/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fs::File;
use std::fs::OpenOptions;
use std::path::Path;
use std::sync::Mutex;

#[cfg(all(unix, not(target_arch = "wasm32")))]
mod platform {
    use std::fs::File;

    use rustix::fs::FlockOperation;
    use rustix::process::Flock;
    use rustix::process::FlockOffsetType;
    use rustix::process::FlockType;

    pub fn grab(file: &File) -> bool {
        rustix::fs::fcntl_lock(file, FlockOperation::NonBlockingLockExclusive).is_ok()
    }

    pub fn check(file: &File) -> bool {
        let lock = Flock {
            start: 0,
            length: 1,
            pid: None,
            typ: FlockType::WriteLock,
            offset_type: FlockOffsetType::Current,
        };
        rustix::process::fcntl_getlk(file, &lock)
            .map(|blocking_lock| blocking_lock.is_none())
            .unwrap_or(false)
    }
}

#[cfg(not(all(unix, not(target_arch = "wasm32"))))]
mod platform {
    use std::fs::File;

    pub fn grab(file: &File) -> bool {
        file.try_lock().is_ok()
    }

    pub fn check(file: &File) -> bool {
        match file.try_lock() {
            Ok(()) => file.unlock().is_ok(),
            Err(_) => false,
        }
    }
}

static LOCKS: Mutex<Option<HashMap<String, File>>> = Mutex::new(None);

// Grabs the file lock and returns true if it the lock was grabbed
pub fn grab(lock_file: &str) -> bool {
    let mut locks = LOCKS.lock().unwrap();
    let locks = locks.get_or_insert_with(HashMap::new);
    if locks.contains_key(lock_file) {
        return true;
    }

    if let Some(parent) = Path::new(lock_file).parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    let file = match OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(lock_file)
    {
        Ok(file) => file,
        Err(_) => return false,
    };
    if platform::grab(&file) {
        locks.insert(lock_file.to_string(), file);
        true
    } else {
        false
    }
}

// Check if the file lock is available without grabbing it.
// Returns true if the lock is free.
pub fn check(lock_file: &str) -> bool {
    if LOCKS
        .lock()
        .unwrap()
        .as_ref()
        .is_some_and(|locks| locks.contains_key(lock_file))
    {
        return true;
    }

    if let Some(parent) = Path::new(lock_file).parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    let file = match OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(lock_file)
    {
        Ok(file) => file,
        Err(_) => return false,
    };
    platform::check(&file)
}
