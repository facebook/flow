/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Entity type for versioned storage with committed and latest values.
//!
//! This module provides the `Entity` type which stores two slots and uses a
//! shared transaction version to decide which slot is committed.

use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use dupe::Dupe;

pub use crate::resolved_requires::Dependency;
pub use crate::resolved_requires::DependencyTarget;
pub use crate::resolved_requires::ResolvedModule;
pub use crate::resolved_requires::ResolvedRequires;

#[derive(Debug, Clone, Dupe)]
pub(crate) struct EntityTransaction {
    next_version: Arc<AtomicU64>,
}

impl EntityTransaction {
    pub(crate) fn new() -> Self {
        Self {
            next_version: Arc::new(AtomicU64::new(2)),
        }
    }

    pub(crate) fn commit(&self) {
        self.next_version.fetch_add(2, Ordering::AcqRel);
    }

    fn next_version(&self) -> u64 {
        self.next_version.load(Ordering::Acquire)
    }
}

impl Default for EntityTransaction {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub(crate) struct Entity<T> {
    transaction: EntityTransaction,
    slots: [parking_lot::RwLock<Option<Box<T>>>; 2],
    version: AtomicU64,
}

impl<T> Entity<T> {
    pub(crate) fn new(transaction: EntityTransaction, value: T) -> Self {
        let version = transaction.next_version();
        Self {
            transaction,
            slots: [
                parking_lot::RwLock::new(Some(Box::new(value))),
                parking_lot::RwLock::new(None),
            ],
            version: AtomicU64::new(version),
        }
    }

    pub(crate) fn new_committed(transaction: EntityTransaction, value: T) -> Self {
        let version = transaction.next_version().saturating_sub(2);
        Self {
            transaction,
            slots: [
                parking_lot::RwLock::new(Some(Box::new(value))),
                parking_lot::RwLock::new(None),
            ],
            version: AtomicU64::new(version),
        }
    }

    pub(crate) fn empty(transaction: EntityTransaction) -> Self {
        let version = transaction.next_version();
        Self {
            transaction,
            slots: [
                parking_lot::RwLock::new(None),
                parking_lot::RwLock::new(None),
            ],
            version: AtomicU64::new(version),
        }
    }

    pub(crate) fn empty_committed(transaction: EntityTransaction) -> Self {
        let version = transaction.next_version().saturating_sub(2);
        Self {
            transaction,
            slots: [
                parking_lot::RwLock::new(None),
                parking_lot::RwLock::new(None),
            ],
            version: AtomicU64::new(version),
        }
    }

    pub(crate) fn set(&self, new_value: T) {
        self.advance(Some(new_value));
    }

    pub(crate) fn advance(&self, new_value_opt: Option<T>) {
        let new_value_opt = new_value_opt.map(Box::new);
        let next_version = self.transaction.next_version();
        let entity_version = self.version.load(Ordering::Acquire);
        let slot = if entity_version < next_version {
            let slot = 1 - (entity_version & 1) as usize;
            *self.slots[slot].write() = new_value_opt;
            self.version
                .store(next_version | slot as u64, Ordering::Release);
            return;
        } else {
            (entity_version & 1) as usize
        };
        *self.slots[slot].write() = new_value_opt;
    }

    pub(crate) fn read_committed(&self) -> Option<T>
    where
        T: Dupe,
    {
        let next_version = self.transaction.next_version();
        let entity_version = self.version.load(Ordering::Acquire);
        let committed_version = if entity_version >= next_version {
            !entity_version
        } else {
            entity_version
        };
        let slot = (committed_version & 1) as usize;
        self.slots[slot].read().as_deref().map(|v| v.dupe())
    }

    pub(crate) fn read_latest(&self) -> Option<T>
    where
        T: Dupe,
    {
        let slot = (self.version.load(Ordering::Acquire) & 1) as usize;
        self.slots[slot].read().as_deref().map(|v| v.dupe())
    }

    pub(crate) fn read_latest_clone(&self) -> Option<T>
    where
        T: Dupe,
    {
        self.read_latest()
    }

    pub(crate) fn read_committed_clone(&self) -> Option<T>
    where
        T: Dupe,
    {
        self.read_committed()
    }

    pub(crate) fn rollback(&self)
    where
        T: Dupe,
    {
        let next_version = self.transaction.next_version();
        let entity_version = self.version.load(Ordering::Acquire);
        let diff = if entity_version == next_version {
            1
        } else if entity_version > next_version {
            3
        } else {
            0
        };
        if diff > 0 {
            self.version.store(entity_version - diff, Ordering::Release);
        }
    }

    pub(crate) fn has_changed(&self) -> bool {
        self.version.load(Ordering::Acquire) >= self.transaction.next_version()
    }

    pub(crate) fn get(&self) -> T
    where
        T: Dupe,
    {
        let slot = (self.version.load(Ordering::Acquire) & 1) as usize;
        self.slots[slot]
            .read()
            .as_deref()
            .expect("Entity new value should be set")
            .dupe()
    }
}
