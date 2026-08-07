/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Replace-capable overlays built on Flow's insertion-only lock-free map.

use std::collections::HashMap;
use std::collections::hash_map::RandomState;
use std::hash::BuildHasher;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::OnceLock;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use dupe::Dupe;
use hashbrown::HashMap as HashbrownMap;
use hashbrown::hash_map::RawEntryMut;
use parking_lot::Mutex;
#[cfg(not(target_arch = "wasm32"))]
use rayon::prelude::*;

use crate::locked_map::LockedMap;
use crate::with_hash::WithHash;

const COMMIT_SHARDS: usize = 1024;

static COMMIT_HASH_BUILDER: OnceLock<RandomState> = OnceLock::new();

#[derive(Debug, Clone)]
pub struct CommitBuildHasher(RandomState);

impl Default for CommitBuildHasher {
    fn default() -> Self {
        Self(COMMIT_HASH_BUILDER.get_or_init(RandomState::new).clone())
    }
}

impl BuildHasher for CommitBuildHasher {
    type Hasher = <RandomState as BuildHasher>::Hasher;

    fn build_hasher(&self) -> Self::Hasher {
        self.0.build_hasher()
    }
}

pub type CommitMap<K, V> = HashbrownMap<K, V, CommitBuildHasher>;

pub fn commit_map_with_capacity<K, V>(capacity: usize) -> CommitMap<K, V> {
    CommitMap::with_capacity_and_hasher(capacity, CommitBuildHasher::default())
}

fn commit_hash<K: Hash + ?Sized>(key: &K) -> u64 {
    CommitBuildHasher::default().hash_one(key)
}

pub fn commit_map_insert_hashed<K, V>(map: &mut CommitMap<K, V>, hash: u64, key: K, value: V)
where
    K: Eq + Hash,
{
    match map
        .raw_entry_mut()
        .from_hash(hash, |candidate| candidate == &key)
    {
        RawEntryMut::Occupied(mut entry) => {
            entry.insert(value);
        }
        RawEntryMut::Vacant(entry) => {
            entry.insert_hashed_nocheck(hash, key, value);
        }
    }
}

pub fn commit_map_remove_hashed<K, V>(map: &mut CommitMap<K, V>, hash: u64, key: &K)
where
    K: Eq + Hash,
{
    if let RawEntryMut::Occupied(entry) = map
        .raw_entry_mut()
        .from_hash(hash, |candidate| candidate == key)
    {
        entry.remove_entry();
    }
}

fn overlay_commit_shard_index<K>(key: &K) -> usize
where
    K: Hash,
{
    WithHash::new(key).hash() as usize & (COMMIT_SHARDS - 1)
}

#[derive(Debug, Clone, Dupe)]
pub enum OverlayValue<V> {
    Present(V),
    Deleted,
}

#[derive(Debug)]
pub enum OverlayMapCommitValue<V> {
    Present(V),
    Deleted,
}

#[derive(Debug)]
pub struct DrainedOverlayMap<K, V> {
    entries: Vec<(u64, K, OverlayMapCommitValue<V>)>,
}

impl<K, V> DrainedOverlayMap<K, V> {
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn into_entries(self) -> impl Iterator<Item = (u64, K, OverlayMapCommitValue<V>)> {
        self.entries.into_iter()
    }
}

#[derive(Debug)]
pub struct LockfreeOverlayMap<K, V> {
    cells: LockedMap<K, Mutex<OverlayValue<V>>>,
    present_count: AtomicUsize,
    mutation_generation: AtomicUsize,
    cached_count: Mutex<Option<(usize, usize)>>,
}

impl<K, V> Default for LockfreeOverlayMap<K, V> {
    fn default() -> Self {
        Self {
            cells: LockedMap::new(),
            present_count: AtomicUsize::new(0),
            mutation_generation: AtomicUsize::new(0),
            cached_count: Mutex::new(None),
        }
    }
}

impl<K, V> LockfreeOverlayMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }

    pub fn len(&self) -> usize {
        self.cells.len()
    }

    pub fn take_commit_entries(&mut self) -> DrainedOverlayMap<K, V>
    where
        K: Eq + Hash + Send,
        V: Send,
    {
        let entries = std::mem::take(&mut self.cells)
            .into_entries()
            .collect::<Vec<_>>();
        let to_commit_entry = |(key, cell): (K, Mutex<OverlayValue<V>>)| {
            let hash = commit_hash(&key);
            let value = match cell.into_inner() {
                OverlayValue::Present(value) => OverlayMapCommitValue::Present(value),
                OverlayValue::Deleted => OverlayMapCommitValue::Deleted,
            };
            (hash, key, value)
        };
        #[cfg(target_arch = "wasm32")]
        let entries = entries.into_iter().map(to_commit_entry).collect();
        #[cfg(not(target_arch = "wasm32"))]
        let entries = entries.into_par_iter().map(to_commit_entry).collect();
        DrainedOverlayMap { entries }
    }

    pub fn clear_latest_entries_parallel(&mut self)
    where
        K: Send,
        V: Send + Sync,
    {
        let entries = std::mem::take(&mut self.cells)
            .into_entries()
            .collect::<Vec<_>>();
        #[cfg(target_arch = "wasm32")]
        entries.into_iter().for_each(drop);
        #[cfg(not(target_arch = "wasm32"))]
        entries.into_par_iter().with_min_len(4096).for_each(drop);
    }
}

impl<K, V> LockfreeOverlayMap<K, V>
where
    K: Eq + Hash + Dupe + 'static,
    V: Dupe + 'static,
{
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.cells.keys()
    }

    pub fn count_over<BaseV>(&self, base: &CommitMap<K, BaseV>) -> usize {
        if base.is_empty() {
            return self.present_count.load(Ordering::Acquire);
        }
        if self.cells.is_empty() {
            return base.len();
        }
        let generation = self.mutation_generation.load(Ordering::Acquire);
        if let Some((cached_generation, count)) = *self.cached_count.lock()
            && cached_generation == generation
        {
            return count;
        }
        let mut count = base.len();
        for (key, value) in self.iter() {
            match value {
                OverlayValue::Present(_) => {
                    if !base.contains_key(&key) {
                        count += 1;
                    }
                }
                OverlayValue::Deleted => {
                    if base.contains_key(&key) {
                        count -= 1;
                    }
                }
            }
        }
        if generation == self.mutation_generation.load(Ordering::Acquire) {
            *self.cached_count.lock() = Some((generation, count));
        }
        count
    }

    pub fn remove(&self, key: K) {
        let cell = self
            .cells
            .ensure(&key, || Mutex::new(OverlayValue::Deleted));
        let old = std::mem::replace(&mut *cell.lock(), OverlayValue::Deleted);
        if matches!(old, OverlayValue::Present(_)) {
            self.present_count.fetch_sub(1, Ordering::Release);
        }
        self.mutation_generation.fetch_add(1, Ordering::Release);
    }

    pub fn get(&self, key: &K) -> Option<OverlayValue<V>> {
        self.cells.get(key).map(|cell| cell.lock().dupe())
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, OverlayValue<V>)> + '_ {
        self.cells
            .iter_unordered()
            .map(|(key, cell)| (key.dupe(), cell.lock().dupe()))
    }
}

impl<K, V> LockfreeOverlayMap<K, V>
where
    K: Eq + Hash + Dupe + 'static,
    V: Dupe + 'static,
{
    pub fn insert(&self, key: K, value: V) {
        let cell = self
            .cells
            .ensure(&key, || Mutex::new(OverlayValue::Deleted));
        let old = std::mem::replace(&mut *cell.lock(), OverlayValue::Present(value));
        if matches!(old, OverlayValue::Deleted) {
            self.present_count.fetch_add(1, Ordering::Release);
        }
        self.mutation_generation.fetch_add(1, Ordering::Release);
    }
}

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub enum SetOp {
    Add,
    Remove,
}

#[derive(Debug)]
pub enum SetOwnerOps<V> {
    Adds(Vec<V>),
    Ops(Vec<(V, SetOp)>),
}

#[derive(Debug)]
pub struct DrainedSetOverlay<K, V> {
    shards: Vec<HashMap<K, CommitSetDeltaValues<V>>>,
    owner_count: usize,
}

impl<K, V> DrainedSetOverlay<K, V> {
    pub fn owner_count(&self) -> usize {
        self.owner_count
    }

    pub fn is_empty(&self) -> bool {
        self.owner_count == 0
    }
}

impl<K, V> DrainedSetOverlay<K, V>
where
    V: Eq + Hash + Dupe,
{
    pub fn into_arc_owner_groups_for_empty_base(self) -> impl Iterator<Item = (K, Arc<Vec<V>>)> {
        self.shards
            .into_iter()
            .flat_map(HashMap::into_iter)
            .filter_map(|(owner, values)| match values {
                CommitSetDeltaValues::Adds(adds) => Some((owner, adds)),
                CommitSetDeltaValues::Ops(ops) => {
                    let values = dedup_owned_ops(ops)
                        .into_iter()
                        .filter_map(|(value, op)| match op {
                            SetOp::Add => Some(value),
                            SetOp::Remove => None,
                        })
                        .collect::<Vec<_>>();
                    (!values.is_empty()).then(|| (owner, Arc::new(values)))
                }
            })
    }

    pub fn into_owner_groups(self) -> impl Iterator<Item = (K, Vec<(V, SetOp)>)> {
        self.shards
            .into_iter()
            .flat_map(HashMap::into_iter)
            .map(|(owner, values)| match values {
                CommitSetDeltaValues::Adds(adds) => (
                    owner,
                    arc_into_vec(adds)
                        .into_iter()
                        .map(|value| (value, SetOp::Add))
                        .collect(),
                ),
                CommitSetDeltaValues::Ops(ops) => (owner, dedup_owned_ops(ops)),
            })
    }
}

#[derive(Debug)]
struct CommitSetOverlay<K, V> {
    shards: [Mutex<HashMap<K, CommitSetDeltaValues<V>>>; COMMIT_SHARDS],
}

impl<K, V> Default for CommitSetOverlay<K, V> {
    fn default() -> Self {
        Self {
            shards: std::array::from_fn(|_| Mutex::new(HashMap::new())),
        }
    }
}

impl<K, V> CommitSetOverlay<K, V> {
    fn new() -> Self {
        Self::default()
    }

    fn owner_count(&self) -> usize {
        self.shards.iter().map(|shard| shard.lock().len()).sum()
    }
}

impl<K, V> CommitSetOverlay<K, V>
where
    K: Eq + Hash,
    V: Eq + Hash + Dupe,
{
    fn shard_index(key: &K) -> usize {
        overlay_commit_shard_index(key)
    }

    fn push(&self, owner: K, value: V, op: SetOp) {
        let mut shard = self.shards[Self::shard_index(&owner)].lock();
        push_commit_set_delta_values(
            shard
                .entry(owner)
                .or_insert_with(|| CommitSetDeltaValues::Adds(Arc::new(Vec::new()))),
            value,
            op,
        );
    }

    fn take(&self) -> DrainedSetOverlay<K, V> {
        let mut owner_count = 0;
        let shards = self
            .shards
            .iter()
            .map(|shard| {
                let map = std::mem::take(&mut *shard.lock());
                owner_count += map.len();
                map
            })
            .collect();
        DrainedSetOverlay {
            shards,
            owner_count,
        }
    }

    fn iter_owner_groups(&self) -> Vec<(K, Vec<(V, SetOp)>)>
    where
        K: Dupe,
    {
        self.shards
            .iter()
            .flat_map(|shard| {
                shard
                    .lock()
                    .iter()
                    .map(|(owner, values)| (owner.dupe(), commit_set_delta_values_ops(values)))
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn owner_keys(&self) -> Vec<K>
    where
        K: Dupe,
    {
        self.shards
            .iter()
            .flat_map(|shard| shard.lock().keys().map(Dupe::dupe).collect::<Vec<_>>())
            .collect()
    }

    fn iter_owner_groups_for_empty_base(&self) -> Vec<(K, SetOwnerOps<V>)>
    where
        K: Dupe,
    {
        self.shards
            .iter()
            .flat_map(|shard| {
                shard
                    .lock()
                    .iter()
                    .map(|(owner, values)| {
                        (
                            owner.dupe(),
                            commit_set_delta_values_ops_for_empty_base(values),
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn iter_for_owner(&self, owner: &K) -> Vec<(V, SetOp)> {
        self.shards[Self::shard_index(owner)]
            .lock()
            .get(owner)
            .map(commit_set_delta_values_ops)
            .unwrap_or_default()
    }
}

#[derive(Debug)]
enum CommitSetDeltaValues<V> {
    Adds(Arc<Vec<V>>),
    Ops(Vec<(V, SetOp)>),
}

fn commit_set_delta_values_ops<V>(values: &CommitSetDeltaValues<V>) -> Vec<(V, SetOp)>
where
    V: Eq + Hash + Dupe,
{
    match values {
        CommitSetDeltaValues::Adds(adds) => adds
            .iter()
            .map(|value| (value.dupe(), SetOp::Add))
            .collect(),
        CommitSetDeltaValues::Ops(ops) => dedup_ops(ops),
    }
}

fn commit_set_delta_values_ops_for_empty_base<V>(values: &CommitSetDeltaValues<V>) -> SetOwnerOps<V>
where
    V: Eq + Hash + Dupe,
{
    match values {
        CommitSetDeltaValues::Adds(adds) => {
            SetOwnerOps::Adds(adds.iter().map(Dupe::dupe).collect())
        }
        CommitSetDeltaValues::Ops(ops) => SetOwnerOps::Ops(dedup_ops(ops)),
    }
}

fn push_commit_set_delta_values<V>(values: &mut CommitSetDeltaValues<V>, value: V, op: SetOp)
where
    V: Dupe,
{
    match op {
        SetOp::Add => match values {
            CommitSetDeltaValues::Adds(adds) => push_arc_add(adds, value),
            CommitSetDeltaValues::Ops(ops) => ops.push((value, SetOp::Add)),
        },
        SetOp::Remove => {
            let old = std::mem::replace(values, CommitSetDeltaValues::Ops(Vec::new()));
            let mut ops = match old {
                CommitSetDeltaValues::Adds(adds) => {
                    let adds = arc_into_vec(adds);
                    let mut ops = Vec::with_capacity(adds.len() + 1);
                    ops.extend(adds.into_iter().map(|value| (value, SetOp::Add)));
                    ops
                }
                CommitSetDeltaValues::Ops(ops) => ops,
            };
            ops.push((value, SetOp::Remove));
            *values = CommitSetDeltaValues::Ops(ops);
        }
    }
}

fn push_arc_add<V>(values: &mut Arc<Vec<V>>, value: V)
where
    V: Dupe,
{
    if let Some(values) = Arc::get_mut(values) {
        values.push(value);
    } else {
        let mut next = Vec::with_capacity(values.len() + 1);
        next.extend(values.iter().map(Dupe::dupe));
        next.push(value);
        *values = Arc::new(next);
    }
}

fn arc_into_vec<V>(values: Arc<Vec<V>>) -> Vec<V>
where
    V: Dupe,
{
    match Arc::try_unwrap(values) {
        Ok(values) => values,
        Err(values) => values.iter().map(Dupe::dupe).collect(),
    }
}

fn dedup_ops<V>(ops: &[(V, SetOp)]) -> Vec<(V, SetOp)>
where
    V: Eq + Hash + Dupe,
{
    let mut final_ops = HashMap::with_capacity(ops.len());
    for (value, op) in ops.iter() {
        final_ops.insert(value.dupe(), *op);
    }
    final_ops.into_iter().collect()
}

fn dedup_owned_ops<V>(ops: Vec<(V, SetOp)>) -> Vec<(V, SetOp)>
where
    V: Eq + Hash,
{
    let mut final_ops = HashMap::with_capacity(ops.len());
    for (value, op) in ops {
        final_ops.insert(value, op);
    }
    final_ops.into_iter().collect()
}

#[derive(Debug)]
pub struct LockfreeSetOverlay<K, V> {
    owners: CommitSetOverlay<K, V>,
}

impl<K, V> Default for LockfreeSetOverlay<K, V> {
    fn default() -> Self {
        Self {
            owners: CommitSetOverlay::new(),
        }
    }
}

impl<K, V> LockfreeSetOverlay<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.owners.owner_count() == 0
    }

    pub fn owner_count(&self) -> usize {
        self.owners.owner_count()
    }

    pub fn owner_keys(&self) -> impl Iterator<Item = K> + '_
    where
        K: Eq + Hash + Dupe + 'static,
        V: Eq + Hash + Dupe + 'static,
    {
        self.owners.owner_keys().into_iter()
    }
}

impl<K, V> LockfreeSetOverlay<K, V>
where
    K: Eq + Hash + Dupe + 'static,
    V: Eq + Hash + Dupe + 'static,
{
    pub fn add(&self, owner: K, value: V) {
        self.set(owner, value, SetOp::Add);
    }

    pub fn remove(&self, owner: K, value: V) {
        self.set(owner, value, SetOp::Remove);
    }

    fn set(&self, owner: K, value: V, op: SetOp) {
        self.owners.push(owner, value, op);
    }

    #[cfg(test)]
    pub fn get(&self, owner: &K, value: &V) -> Option<SetOp> {
        self.owners
            .iter_for_owner(owner)
            .into_iter()
            .rev()
            .find_map(|(candidate, op)| (&candidate == value).then_some(op))
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, V, SetOp)> + '_ {
        self.iter_owner_groups().flat_map(|(owner, values)| {
            values
                .into_iter()
                .map(move |(value, op)| (owner.dupe(), value, op))
        })
    }

    pub fn iter_owner_groups(&self) -> impl Iterator<Item = (K, Vec<(V, SetOp)>)> + '_ {
        self.owners.iter_owner_groups().into_iter()
    }

    pub fn iter_owner_groups_for_empty_base(
        &self,
    ) -> impl Iterator<Item = (K, SetOwnerOps<V>)> + '_ {
        self.owners.iter_owner_groups_for_empty_base().into_iter()
    }

    pub fn take_commit_owner_groups(&self) -> DrainedSetOverlay<K, V> {
        self.owners.take()
    }

    pub fn iter_for_owner(&self, owner: &K) -> impl Iterator<Item = (V, SetOp)> + '_ {
        self.owners.iter_for_owner(owner).into_iter()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Barrier;

    use super::*;

    #[test]
    fn overlay_map_replaces_values() {
        let map = LockfreeOverlayMap::new();
        map.insert("a", 1);
        map.insert("a", 2);

        match map.get(&"a") {
            Some(OverlayValue::Present(value)) => assert_eq!(value, 2),
            other => panic!("unexpected overlay value: {other:?}"),
        }
    }

    #[test]
    fn overlay_map_records_deletions() {
        let map = LockfreeOverlayMap::new();
        map.insert("a", 1);
        map.remove("a");

        assert!(matches!(map.get(&"a"), Some(OverlayValue::Deleted)));
    }

    #[test]
    fn overlay_map_caches_exact_count_over_base() {
        let map = LockfreeOverlayMap::new();
        let mut base = CommitMap::default();
        base.insert("existing", 1);
        base.insert("removed", 2);

        assert_eq!(map.count_over(&base), 2);
        map.insert("existing", 3);
        map.insert("new", 4);
        map.remove("removed");
        assert_eq!(map.count_over(&base), 2);

        map.remove("new");
        assert_eq!(map.count_over(&base), 1);
        map.insert("removed", 5);
        assert_eq!(map.count_over(&base), 2);
    }

    #[test]
    fn overlay_map_counts_empty_base_without_scanning() {
        let map = LockfreeOverlayMap::new();
        let base = CommitMap::<&str, i32>::default();

        map.insert("first", 1);
        map.insert("second", 2);
        map.insert("first", 3);
        map.remove("second");

        assert_eq!(map.count_over(&base), 1);
    }

    #[test]
    fn set_overlay_preserves_last_operation() {
        let set = LockfreeSetOverlay::new();
        set.add("owner", "value");
        set.remove("owner", "value");
        assert_eq!(set.get(&"owner", &"value"), Some(SetOp::Remove));

        set.add("owner", "value");
        assert_eq!(set.get(&"owner", &"value"), Some(SetOp::Add));
    }

    #[test]
    fn map_latest_value_matches_commit_value_after_concurrent_writes() {
        let map = Arc::new(LockfreeOverlayMap::new());
        let barrier = Arc::new(Barrier::new(9));
        let threads = (0..8)
            .map(|value| {
                let map = Arc::clone(&map);
                let barrier = Arc::clone(&barrier);
                std::thread::spawn(move || {
                    barrier.wait();
                    map.insert("key", value);
                })
            })
            .collect::<Vec<_>>();
        barrier.wait();
        for thread in threads {
            thread.join().expect("writer thread should finish");
        }

        let mut map = Arc::try_unwrap(map).expect("writer handles should be dropped");
        let latest = match map.get(&"key") {
            Some(OverlayValue::Present(value)) => value,
            other => panic!("unexpected overlay value: {other:?}"),
        };
        let committed = map
            .take_commit_entries()
            .into_entries()
            .find_map(|(_hash, key, value)| {
                (key == "key").then_some(match value {
                    OverlayMapCommitValue::Present(value) => value,
                    OverlayMapCommitValue::Deleted => panic!("key should be present"),
                })
            })
            .expect("commit entry should exist");
        assert_eq!(latest, committed);
    }
}
