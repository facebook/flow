/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Entity type for versioned storage with old and new values.
//!
//! This module provides the `Entity` type which stores both old and new values
//! for tracking changes to mutable fields in the heap.

use dupe::Dupe;

pub use crate::resolved_requires::Dependency;
pub use crate::resolved_requires::ResolvedModule;
pub use crate::resolved_requires::ResolvedRequires;

#[derive(Debug, Clone)]
pub struct Entity<T> {
    old: std::sync::Arc<parking_lot::RwLock<Option<T>>>,
    new: std::sync::Arc<parking_lot::RwLock<Option<T>>>,
}

impl<T> Entity<T> {
    pub fn new(value: T) -> Self {
        Self {
            old: std::sync::Arc::new(parking_lot::RwLock::new(None)),
            new: std::sync::Arc::new(parking_lot::RwLock::new(Some(value))),
        }
    }

    pub fn empty() -> Self {
        Self {
            old: std::sync::Arc::new(parking_lot::RwLock::new(None)),
            new: std::sync::Arc::new(parking_lot::RwLock::new(None)),
        }
    }

    pub fn set(&self, new_value: T) {
        let current = self.new.write().take();
        *self.old.write() = current;
        *self.new.write() = Some(new_value);
    }

    pub fn advance(&self, new_value_opt: Option<T>) {
        let current = self.new.write().take();
        *self.old.write() = current;
        *self.new.write() = new_value_opt;
    }

    pub fn read_committed(&self) -> Option<T>
    where
        T: Dupe,
    {
        self.old.read().as_ref().map(|v| v.dupe())
    }

    pub fn read_latest(&self) -> Option<T>
    where
        T: Dupe,
    {
        self.new.read().as_ref().map(|v| v.dupe())
    }

    pub fn read_latest_clone(&self) -> Option<T>
    where
        T: Dupe,
    {
        self.new.read().dupe()
    }

    pub fn read_committed_clone(&self) -> Option<T>
    where
        T: Dupe,
    {
        self.old.read().dupe()
    }

    pub fn has_changed(&self) -> bool
    where
        T: PartialEq,
    {
        let old = self.old.read();
        let new = self.new.read();
        *old != *new
    }

    pub fn get(&self) -> T
    where
        T: Dupe,
    {
        self.new
            .read()
            .as_ref()
            .expect("Entity new value should be set")
            .dupe()
    }
}
