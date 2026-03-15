/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! In Rust, the std synchronization primitives in Rust use a [poisoning technique](https://doc.rust-lang.org/nomicon/poisoning.html).
//! For example, if an exception occurs when a lock is held, the lock is poisoned before being released as the stack unwinds.
//! Without poisoning, another thread could enter the lock and observe an invalid state.
//!
//! If we have a panic, we immediately terminate the program, so we should never encounter a poisoned lock.
//! These wrappers just panic if we encounter a poisoned lock.

use std::sync;

#[derive(Debug, Default)]
pub struct Mutex<T>(sync::Mutex<T>);

impl<T> Mutex<T> {
    pub fn new(t: T) -> Self {
        Self(sync::Mutex::new(t))
    }

    pub fn lock(&self) -> sync::MutexGuard<'_, T> {
        self.0.lock().unwrap()
    }

    pub fn try_lock(&self) -> Option<sync::MutexGuard<'_, T>> {
        self.0.try_lock().ok()
    }

    pub fn into_inner(self) -> T {
        self.0.into_inner().unwrap()
    }
}

#[derive(Debug, Default)]
pub struct RwLock<T>(sync::RwLock<T>);

impl<T> RwLock<T> {
    pub fn new(t: T) -> Self {
        Self(sync::RwLock::new(t))
    }

    pub fn read(&self) -> sync::RwLockReadGuard<'_, T> {
        self.0.read().unwrap()
    }

    pub fn write(&self) -> sync::RwLockWriteGuard<'_, T> {
        self.0.write().unwrap()
    }

    pub fn into_inner(self) -> T {
        self.0.into_inner().unwrap()
    }
}

#[derive(Debug, Default)]
pub struct Condvar(sync::Condvar);

impl Condvar {
    pub fn new() -> Self {
        Self(sync::Condvar::new())
    }

    pub fn notify_one(&self) {
        self.0.notify_one();
    }

    pub fn notify_all(&self) {
        self.0.notify_all();
    }

    pub fn wait_while<'a, T, F>(
        &self,
        guard: sync::MutexGuard<'a, T>,
        condition: F,
    ) -> sync::MutexGuard<'a, T>
    where
        F: FnMut(&mut T) -> bool,
    {
        self.0.wait_while(guard, condition).unwrap()
    }

    pub fn wait_timeout<'a, T>(
        &self,
        guard: sync::MutexGuard<'a, T>,
        duration: std::time::Duration,
    ) -> (sync::MutexGuard<'a, T>, sync::WaitTimeoutResult) {
        self.0.wait_timeout(guard, duration).unwrap()
    }
}
