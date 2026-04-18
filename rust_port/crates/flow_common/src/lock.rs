/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;
use std::sync::atomic::AtomicI32;
use std::sync::atomic::Ordering;

struct LockEntry {
    file: std::fs::File,
    raw_id: i32,
}

static LOCK_FDS: Mutex<Option<HashMap<String, LockEntry>>> = Mutex::new(None);
static NEXT_LOCK_ID: AtomicI32 = AtomicI32::new(0);

fn lock_fds_with<R>(f: impl FnOnce(&mut HashMap<String, LockEntry>) -> R) -> R {
    let mut guard = LOCK_FDS.lock().unwrap();
    if guard.is_none() {
        *guard = Some(HashMap::new());
    }
    f(guard.as_mut().unwrap())
}

fn open_lock_file(lock_file: &str) -> std::io::Result<std::fs::File> {
    std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .open(lock_file)
}

fn next_lock_id() -> i32 {
    NEXT_LOCK_ID.fetch_add(1, Ordering::Relaxed) + 1
}

fn make_lock_entry(file: std::fs::File) -> LockEntry {
    LockEntry {
        raw_id: next_lock_id(),
        file,
    }
}

fn normalize_try_lock_error(err: std::fs::TryLockError) -> std::io::Result<()> {
    match err {
        std::fs::TryLockError::WouldBlock => Err(std::io::Error::new(
            std::io::ErrorKind::WouldBlock,
            "would block",
        )),
        std::fs::TryLockError::Error(e) => Err(e),
    }
}

// Basic lock operations.
//
// We use these for two reasons:
// 1. making sure we are only running one instance of hh_server per person on a given dev box
// 2. giving a way to hh_client to check if a server is running.
fn register_lock(lock_file: &str) -> std::io::Result<()> {
    if let Some(parent) = Path::new(lock_file).parent() {
        let _mkdir_result = std::fs::create_dir_all(parent);
    }
    let file = open_lock_file(lock_file)?;
    let entry = make_lock_entry(file);
    lock_fds_with(|map| {
        map.insert(lock_file.to_string(), entry);
    });
    Ok(())
}

// Grab or check if a file lock is available.
//
// Returns true if the lock is/was available, false otherwise.
fn _operations(lock_file: &str, op: LockOp) -> bool {
    let result: Result<(), ()> = (|| {
        let already_registered = lock_fds_with(|map| map.contains_key(lock_file));
        match (already_registered, Path::new(lock_file).exists()) {
            (false, _) => register_lock(lock_file).map_err(|_| ())?,
            (true, false) => return Err(()),
            (true, true) => {}
        }
        lock_fds_with(|map| -> Result<(), ()> {
            let entry = map.get(lock_file).ok_or(())?;
            apply_lock_op(&entry.file, op).map_err(|_| ())
        })
    })();
    result.is_ok()
}

#[derive(Clone, Copy)]
enum LockOp {
    TLock,
    ULock,
    Lock,
    Test,
}

fn try_lock_or_test(file: &std::fs::File, op: LockOp) -> std::io::Result<()> {
    match file.try_lock() {
        Ok(()) => {
            if let LockOp::Test = op {
                file.unlock()?;
            }
            Ok(())
        }
        Err(err) => normalize_try_lock_error(err),
    }
}

fn apply_lock_op(file: &std::fs::File, op: LockOp) -> std::io::Result<()> {
    match op {
        LockOp::TLock => try_lock_or_test(file, op),
        LockOp::ULock => file.unlock(),
        LockOp::Lock => file.lock(),
        LockOp::Test => try_lock_or_test(file, op),
    }
}

// Grabs the file lock and returns true if it the lock was grabbed
pub fn grab(lock_file: &str) -> bool {
    _operations(lock_file, LockOp::TLock)
}

// Releases a file lock.
pub fn release(lock_file: &str) -> bool {
    _operations(lock_file, LockOp::ULock)
}

pub fn blocking_grab_then_release(lock_file: &str) {
    let _grabbed = _operations(lock_file, LockOp::Lock);
    let _released = release(lock_file);
}

// Gets the server instance-unique integral fd for a given lock file.
pub fn fd_of(lock_file: &str) -> i32 {
    lock_fds_with(|map| match map.get(lock_file) {
        None => -1,
        Some(entry) => entry.raw_id,
    })
}

// Check if the file lock is available without grabbing it.
// Returns true if the lock is free.
pub fn check(lock_file: &str) -> bool {
    _operations(lock_file, LockOp::Test)
}
