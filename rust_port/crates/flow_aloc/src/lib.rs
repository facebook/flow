/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(never_type)]

use std::cell::LazyCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::rc::Rc;

use dupe::Dupe;
use flow_packed_locs::packed_locs;
use flow_parser::file_key::FileKey;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::loc_sig::LocSig;
use flow_parser::polymorphic_ast_mapper::LocMapper;

pub type LazyALocTable = Rc<LazyCell<Rc<ALocTable>, Box<dyn FnOnce() -> Rc<ALocTable>>>>;

/// Mapper for converting Loc to ALoc in AST transformations.
pub struct LocToALocMapper;

impl LocMapper<Loc, Loc, ALoc, ALoc> for LocToALocMapper {
    fn on_loc_annot(&mut self, loc: &Loc) -> Result<ALoc, !> {
        Ok(ALoc::of_loc(loc.dupe()))
    }

    fn on_type_annot(&mut self, annot: &Loc) -> Result<ALoc, !> {
        Ok(ALoc::of_loc(annot.dupe()))
    }
}

pub struct ALocTable {
    // This is not strictly necessary, but it allows us to check that the location source matches the
    // table source, to avoid confusing issues if we try a lookup with the wrong table.
    file: FileKey,
    locs: Vec<Loc>,
}

impl std::fmt::Debug for ALocTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("file", &self.file)
            .field("locs_count", &self.locs.len())
            .finish()
    }
}

impl ALocTable {
    pub fn empty(file: FileKey) -> Self {
        Self { file, locs: vec![] }
    }
}

#[derive(Debug, Clone, Copy, Dupe, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Key(u32);

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Kind {
    ALocNone,
    Keyed,
    Concrete,
}

/// Sentinel value used in `start.column` to distinguish keyed ALocs from concrete ones.
/// Real column values are non-negative byte offsets, so `i32::MIN` is never a valid column.
const KEYED_SENTINEL: i32 = i32::MIN;

/// Abstract location type. Internally stores a `Loc` (24 bytes), using a sentinel value
/// in `start.column` to distinguish keyed locations from concrete ones.
///
/// For keyed ALocs: `start.line` holds the key (as i32), `start.column == KEYED_SENTINEL`,
/// `end` is zeroed. For concrete ALocs: normal `Loc` data. For none: `LOC_NONE`.
#[derive(Clone, Dupe)]
pub struct ALoc(Loc);

impl std::fmt::Debug for ALoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_keyed() {
            f.debug_struct("ALoc::Keyed")
                .field("source", &self.0.source)
                .field("key", &Key(self.0.start.line as u32))
                .finish()
        } else if self.0.is_none() {
            write!(f, "ALoc::None")
        } else {
            f.debug_tuple("ALoc::Concrete").field(&self.0).finish()
        }
    }
}

fn compare_opt_file_key(a: Option<&FileKey>, b: Option<&FileKey>) -> std::cmp::Ordering {
    match (a, b) {
        (None, None) => std::cmp::Ordering::Equal,
        (None, Some(_)) => std::cmp::Ordering::Less,
        (Some(_), None) => std::cmp::Ordering::Greater,
        (Some(a), Some(b)) => a.cmp(b),
    }
}

impl ALoc {
    pub fn of_loc(loc: Loc) -> Self {
        debug_assert_ne!(
            loc.start.column, KEYED_SENTINEL,
            "Cannot create ALoc from Loc with sentinel column value"
        );
        ALoc(loc)
    }

    fn of_key(source: Option<FileKey>, key: Key) -> Self {
        ALoc(Loc {
            source,
            start: Position {
                line: key.0 as i32,
                column: KEYED_SENTINEL,
            },
            end: Position { line: 0, column: 0 },
        })
    }

    pub fn source(&self) -> Option<&FileKey> {
        self.0.source.as_ref()
    }

    pub fn update_source<F>(&self, f: F) -> ALoc
    where
        F: FnOnce(Option<&FileKey>) -> Option<FileKey>,
    {
        ALoc(Loc {
            source: f(self.0.source.as_ref()),
            start: self.0.start,
            end: self.0.end,
        })
    }

    // The `key` field is an integer in `keyed_t`, but the field in the corresponding location in
    // `Loc.t` is `start`, which is a pointer. We can use this fact to determine whether we have an
    // `keyed_t` or a `t` here, since OCaml keeps track of whether a value is an integer or a
    // pointer. If it's an integer, the value is keyed.
    //
    // In Rust, we use a sentinel value (i32::MIN) in start.column to distinguish keyed from
    // concrete, since real column values are always non-negative byte offsets.
    fn is_keyed(&self) -> bool {
        self.0.start.column == KEYED_SENTINEL
    }

    fn kind(&self) -> Kind {
        if self.is_keyed() {
            Kind::Keyed
        } else if self.0.is_none() {
            Kind::ALocNone
        } else {
            Kind::Concrete
        }
    }

    fn kind_ignore_source(&self) -> Kind {
        if self.is_keyed() {
            Kind::Keyed
        } else if self.0.is_none_ignore_source() {
            Kind::ALocNone
        } else {
            Kind::Concrete
        }
    }

    fn get_key_exn(&self) -> Key {
        if self.is_keyed() {
            Key(self.0.start.line as u32)
        } else {
            panic!("Can only get the key from a keyed location")
        }
    }

    pub fn to_loc_exn(&self) -> &Loc {
        if self.is_keyed() {
            panic!("loc must be concrete")
        } else {
            &self.0
        }
    }

    pub fn to_loc(&self, table: &LazyALocTable) -> Loc {
        if self.is_keyed() {
            let source = self.source();
            let key = self.get_key_exn();
            // let table = Lazy.force table in
            let table: &Rc<ALocTable> = LazyCell::force(&**table);
            if source.map(|s| s == &table.file) == Some(true) {
                table.locs[key.0 as usize].dupe()
            } else {
                panic!("to_loc_safe: File mismatch between location and table")
            }
        } else {
            self.to_loc_exn().dupe()
        }
    }

    pub fn to_loc_with_tables(&self, tables: &HashMap<FileKey, LazyALocTable>) -> Loc {
        if self.is_keyed() {
            let source = self
                .source()
                .expect("Unexpectedly encountered a location without a source");
            let table = tables.get(source).expect("Table not found for source");
            self.to_loc(table)
        } else {
            self.to_loc_exn().dupe()
        }
    }

    pub fn quick_compare(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        // String comparisons are expensive, so we should only evaluate this lambda if
        // the other information we have ties
        let source_compare = || compare_opt_file_key(self.source(), other.source());
        match (self.kind(), other.kind()) {
            (Kind::Keyed, Kind::Keyed) => {
                let k1 = self.get_key_exn();
                let k2 = other.get_key_exn();
                let key_compare = k1.cmp(&k2);
                if key_compare == Equal {
                    source_compare()
                } else {
                    key_compare
                }
            }
            (Kind::Concrete, Kind::Concrete) => {
                let l1 = self.to_loc_exn();
                let l2 = other.to_loc_exn();
                let start_compare = l1.start.cmp(&l2.start);
                if start_compare == Equal {
                    let end_compare = l1.end.cmp(&l2.end);
                    if end_compare == Equal {
                        source_compare()
                    } else {
                        end_compare
                    }
                } else {
                    start_compare
                }
            }
            (Kind::ALocNone, Kind::ALocNone) => Equal,
            (Kind::ALocNone, Kind::Keyed | Kind::Concrete) => Less,
            (Kind::Keyed | Kind::Concrete, Kind::ALocNone) => Greater,
            (Kind::Keyed, Kind::Concrete) => Greater,
            (Kind::Concrete, Kind::Keyed) => Less,
        }
    }

    /// Non-panicking equality check, equivalent to OCaml's structural `=` for ALoc.
    /// Unlike `PartialEq::eq` (which uses `ALoc.compare` and panics on mixed
    /// keyed/concrete), this uses `quick_compare` which never panics.
    pub fn quick_eq(&self, other: &Self) -> bool {
        self.quick_compare(other) == std::cmp::Ordering::Equal
    }

    pub fn concretize_if_possible(
        &self,
        available_tables: &HashMap<FileKey, LazyALocTable>,
    ) -> Self {
        if self.is_keyed() {
            match self.source() {
                // We shouldn't end up with a location with no source and a keyed representation. It may be
                // worth asserting here at some point.
                None => self.dupe(),
                Some(source) => match available_tables.get(source) {
                    // We don't have the right table, so just return the loc
                    None => self.dupe(),
                    // of_loc (to_loc table loc)
                    Some(table) => ALoc::of_loc(self.to_loc(table)),
                },
            }
        } else {
            self.dupe()
        }
    }

    pub fn concretize_compare(
        &self,
        other: &Self,
        available_tables: &HashMap<FileKey, LazyALocTable>,
    ) -> std::cmp::Ordering {
        if self.source() == other.source() && self.is_keyed() != other.is_keyed() {
            let loc1 = self.concretize_if_possible(available_tables);
            let loc2 = other.concretize_if_possible(available_tables);
            loc1.compare(&loc2)
        } else {
            self.compare(other)
        }
    }

    pub fn concretize_equal(
        &self,
        other: &Self,
        available_tables: &HashMap<FileKey, LazyALocTable>,
    ) -> bool {
        self.concretize_compare(other, available_tables) == std::cmp::Ordering::Equal
    }

    pub fn to_string_no_source(&self) -> String {
        if self.is_keyed() {
            let key = self.get_key_exn();
            key.to_string()
        } else {
            self.to_loc_exn().to_string_no_source()
        }
    }
}

impl Default for ALoc {
    fn default() -> Self {
        <ALoc as LocSig>::none()
    }
}

impl LocSig for ALoc {
    fn none() -> Self {
        ALoc(LOC_NONE)
    }

    fn compare(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        let source_compare = compare_opt_file_key(self.source(), other.source());
        if source_compare != Equal {
            return source_compare;
        }
        match (self.kind_ignore_source(), other.kind_ignore_source()) {
            (Kind::Keyed, Kind::Keyed) => {
                let k1 = self.get_key_exn();
                let k2 = other.get_key_exn();
                k1.cmp(&k2)
            }
            (Kind::Concrete, Kind::Concrete) => {
                let l1 = self.to_loc_exn();
                let l2 = other.to_loc_exn();
                l1.compare_ignore_source(l2)
            }
            (Kind::ALocNone, Kind::ALocNone) => Equal,
            (Kind::ALocNone, Kind::Keyed | Kind::Concrete) => Less,
            (Kind::Keyed | Kind::Concrete, Kind::ALocNone) => Greater,
            // This might be too aggressive. For example, we might sort errors by location, and some errors
            // generated about a file might use concrete locations, and others might use keyed ones. For
            // now let's wait and see, and if this is too aggressive we can relax it.
            (Kind::Keyed, Kind::Concrete) | (Kind::Concrete, Kind::Keyed) => {
                panic!(
                    "Unable to compare a keyed location with a concrete one. loc1: {}, loc2: {}",
                    self.debug_to_string(true),
                    other.debug_to_string(true)
                )
            }
        }
    }

    fn equal(&self, other: &Self) -> bool {
        self.compare(other) == std::cmp::Ordering::Equal
    }

    fn debug_to_string(&self, include_source: bool) -> String {
        if self.is_keyed() {
            let source = self.source();
            let key = self.get_key_exn();
            let source_str = if include_source {
                format!("{:?}: ", source.map(|s| s.as_str()).unwrap_or("<NONE>"))
            } else {
                String::new()
            };
            format!("{}{}", source_str, key)
        } else {
            let loc = self.to_loc_exn();
            LocSig::debug_to_string(loc, include_source)
        }
    }
}

#[derive(Debug, Clone, Dupe)]
pub struct ALocId(pub ALoc);

impl ALocId {
    pub fn none() -> Self {
        ALocId(<ALoc as LocSig>::none())
    }

    pub fn of_aloc<F>(aloc: &ALoc, table: F) -> Self
    where
        F: FnOnce() -> Rc<ALocTable>,
    {
        match aloc.kind() {
            Kind::Keyed | Kind::ALocNone => ALocId(aloc.dupe()),
            Kind::Concrete => {
                let table = table();
                let loc = aloc.to_loc_exn();
                match table
                    .locs
                    .binary_search_by(|probe| packed_locs::compare_locs(probe, loc))
                {
                    Ok(key) => match aloc.source() {
                        Some(_) => ALocId(ALoc::of_key(loc.source.dupe(), Key(key as u32))),
                        None => panic!("Unexpectedly encountered a location without a source"),
                    },
                    Err(_) => ALocId(aloc.dupe()),
                }
            }
        }
    }

    pub fn compare(&self, other: &Self) -> std::cmp::Ordering {
        self.0.quick_compare(&other.0)
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.compare(other) == std::cmp::Ordering::Equal
    }
}

impl PartialOrd for ALocId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ALocId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.compare(other)
    }
}

impl PartialEq for ALocId {
    fn eq(&self, other: &Self) -> bool {
        self.compare(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for ALocId {}

impl std::hash::Hash for ALocId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let kind = self.0.kind();
        kind.hash(state);
        match kind {
            Kind::ALocNone => {}
            Kind::Keyed => {
                self.0.get_key_exn().hash(state);
                self.0.source().hash(state);
            }
            Kind::Concrete => {
                let loc = self.0.to_loc_exn();
                loc.start.hash(state);
                loc.end.hash(state);
                self.0.source().hash(state);
            }
        }
    }
}

impl std::fmt::Display for ALocId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Used to power ALocFuzzyMap
#[derive(Debug, Clone, Dupe)]
pub struct ALocFuzzy(pub ALoc);

impl ALocFuzzy {
    pub fn new(aloc: ALoc) -> Self {
        ALocFuzzy(aloc)
    }
}

impl PartialEq for ALocFuzzy {
    fn eq(&self, other: &Self) -> bool {
        self.0.quick_compare(&other.0) == std::cmp::Ordering::Equal
    }
}

impl Eq for ALocFuzzy {}

impl PartialOrd for ALocFuzzy {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ALocFuzzy {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.quick_compare(&other.0)
    }
}

impl std::hash::Hash for ALocFuzzy {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl std::fmt::Display for ALocFuzzy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Internal wrapper that uses LocSig::compare (strict ordering) for key comparison.
/// This ensures ALocMap/ALocSet maintain strict ordering even if ALoc's own Ord changes.
#[derive(Debug, Clone, Dupe)]
struct ALocStrict(ALoc);

impl PartialEq for ALocStrict {
    fn eq(&self, other: &Self) -> bool {
        self.0.compare(&other.0) == std::cmp::Ordering::Equal
    }
}

impl Eq for ALocStrict {}

impl PartialOrd for ALocStrict {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ALocStrict {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.compare(&other.0)
    }
}

impl std::hash::Hash for ALocStrict {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct ALocMap<V>(BTreeMap<ALocStrict, V>);

impl<V> ALocMap<V> {
    pub fn new() -> Self {
        ALocMap(BTreeMap::new())
    }

    pub fn insert(&mut self, key: ALoc, value: V) -> Option<V> {
        self.0.insert(ALocStrict(key), value)
    }

    pub fn get(&self, key: &ALoc) -> Option<&V> {
        self.0.get(&ALocStrict(key.dupe()))
    }

    pub fn get_mut(&mut self, key: &ALoc) -> Option<&mut V> {
        self.0.get_mut(&ALocStrict(key.dupe()))
    }

    pub fn entry(&mut self, key: ALoc) -> ALocMapEntry<'_, V> {
        ALocMapEntry(self.0.entry(ALocStrict(key)))
    }

    pub fn contains_key(&self, key: &ALoc) -> bool {
        self.0.contains_key(&ALocStrict(key.dupe()))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ALoc, &V)> + '_ {
        self.0.iter().map(|(k, v)| (&k.0, v))
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.0.values()
    }
}

pub struct ALocMapEntry<'a, V>(std::collections::btree_map::Entry<'a, ALocStrict, V>);

impl<'a, V> ALocMapEntry<'a, V> {
    pub fn and_modify<F: FnOnce(&mut V)>(self, f: F) -> Self {
        ALocMapEntry(self.0.and_modify(f))
    }

    pub fn or_insert(self, default: V) -> &'a mut V {
        self.0.or_insert(default)
    }

    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
        self.0.or_insert_with(default)
    }
}

impl<V> Default for ALocMap<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> FromIterator<(ALoc, V)> for ALocMap<V> {
    fn from_iter<I: IntoIterator<Item = (ALoc, V)>>(iter: I) -> Self {
        ALocMap(iter.into_iter().map(|(k, v)| (ALocStrict(k), v)).collect())
    }
}

pub struct ALocMapIntoIter<V>(std::collections::btree_map::IntoIter<ALocStrict, V>);

impl<V> Iterator for ALocMapIntoIter<V> {
    type Item = (ALoc, V);
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| (k.0, v))
    }
}

impl<V> IntoIterator for ALocMap<V> {
    type Item = (ALoc, V);
    type IntoIter = ALocMapIntoIter<V>;
    fn into_iter(self) -> Self::IntoIter {
        ALocMapIntoIter(self.0.into_iter())
    }
}

pub struct ALocMapIter<'a, V>(std::collections::btree_map::Iter<'a, ALocStrict, V>);

impl<'a, V> Iterator for ALocMapIter<'a, V> {
    type Item = (&'a ALoc, &'a V);
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| (&k.0, v))
    }
}

impl<'a, V> IntoIterator for &'a ALocMap<V> {
    type Item = (&'a ALoc, &'a V);
    type IntoIter = ALocMapIter<'a, V>;
    fn into_iter(self) -> Self::IntoIter {
        ALocMapIter(self.0.iter())
    }
}

/// Opaque ordered set of ALoc using strict LocSig::compare ordering.
#[derive(Debug, Clone)]
pub struct ALocSet(BTreeSet<ALocStrict>);

impl ALocSet {
    pub fn new() -> Self {
        ALocSet(BTreeSet::new())
    }

    pub fn insert(&mut self, value: ALoc) -> bool {
        self.0.insert(ALocStrict(value))
    }

    pub fn remove(&mut self, value: &ALoc) -> bool {
        self.0.remove(&ALocStrict(value.dupe()))
    }

    pub fn contains(&self, value: &ALoc) -> bool {
        self.0.contains(&ALocStrict(value.dupe()))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ALoc> + '_ {
        self.0.iter().map(|k| &k.0)
    }
}

impl Default for ALocSet {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for ALocSet {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for ALocSet {}

impl FromIterator<ALoc> for ALocSet {
    fn from_iter<I: IntoIterator<Item = ALoc>>(iter: I) -> Self {
        ALocSet(iter.into_iter().map(ALocStrict).collect())
    }
}

impl std::ops::BitOr for &ALocSet {
    type Output = ALocSet;
    fn bitor(self, rhs: Self) -> ALocSet {
        ALocSet(&self.0 | &rhs.0)
    }
}

impl Extend<ALoc> for ALocSet {
    fn extend<I: IntoIterator<Item = ALoc>>(&mut self, iter: I) {
        self.0.extend(iter.into_iter().map(ALocStrict))
    }
}

pub struct ALocSetIntoIter(std::collections::btree_set::IntoIter<ALocStrict>);

impl Iterator for ALocSetIntoIter {
    type Item = ALoc;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|k| k.0)
    }
}

impl IntoIterator for ALocSet {
    type Item = ALoc;
    type IntoIter = ALocSetIntoIter;
    fn into_iter(self) -> Self::IntoIter {
        ALocSetIntoIter(self.0.into_iter())
    }
}

pub struct ALocSetIter<'a>(std::collections::btree_set::Iter<'a, ALocStrict>);

impl<'a> Iterator for ALocSetIter<'a> {
    type Item = &'a ALoc;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|k| &k.0)
    }
}

impl<'a> IntoIterator for &'a ALocSet {
    type Item = &'a ALoc;
    type IntoIter = ALocSetIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        ALocSetIter(self.0.iter())
    }
}

pub type ALocFuzzyMap<V> = BTreeMap<ALocFuzzy, V>;
pub type ALocFuzzySet = BTreeSet<ALocFuzzy>;

pub mod aloc_representation_do_not_use {
    use super::*;

    pub fn is_keyed(loc: &ALoc) -> bool {
        loc.is_keyed()
    }

    pub fn get_key_exn(loc: &ALoc) -> Key {
        loc.get_key_exn()
    }

    pub fn string_of_key(key: Key) -> String {
        key.to_string()
    }

    pub fn make_table(file: FileKey, locs: Vec<Loc>) -> ALocTable {
        ALocTable { file, locs }
    }

    pub fn init_table<F>(file: FileKey, len: usize, mut f: F) -> ALocTable
    where
        F: FnMut() -> Loc,
    {
        let locs = (0..len).map(|_| f()).collect();
        ALocTable { file, locs }
    }

    pub fn make_keyed(source: Option<FileKey>, key: u32) -> ALoc {
        ALoc::of_key(source, Key(key))
    }

    pub fn make_id(source: Option<FileKey>, key: u32) -> ALocId {
        ALocId(ALoc::of_key(source, Key(key)))
    }
}

impl PartialOrd for ALoc {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for ALoc {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.quick_compare(other)
    }
}

impl PartialEq for ALoc {
    fn eq(&self, other: &Self) -> bool {
        self.quick_compare(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for ALoc {}

impl std::hash::Hash for ALoc {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.source().hash(state);
        let kind = self.kind_ignore_source();
        kind.hash(state);
        match kind {
            Kind::ALocNone => {}
            Kind::Keyed => {
                self.get_key_exn().hash(state);
            }
            Kind::Concrete => {
                let loc = self.to_loc_exn();
                loc.start.hash(state);
                loc.end.hash(state);
            }
        }
    }
}

impl std::fmt::Display for ALoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string_no_source())
    }
}

impl ALocTable {
    pub fn pack(&self) -> PackedALocTable {
        PackedALocTable(packed_locs::pack(self.locs.len(), self.locs.iter()))
    }

    pub fn unpack(file: FileKey, packed: &PackedALocTable) -> Self {
        let locs: Vec<Loc> = packed_locs::unpack(
            Some(file.dupe()),
            |len, f| (0..len).map(f).collect(),
            &packed.0,
        );
        Self { file, locs }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackedALocTable(Vec<u8>);

#[cfg(test)]
mod tests {
    use flow_parser::file_key::FileKeyInner;
    use flow_parser::loc::Position;

    use super::*;

    #[test]
    fn aloc_size_equals_loc_size() {
        assert_eq!(
            std::mem::size_of::<ALoc>(),
            std::mem::size_of::<Loc>(),
            "ALoc should be the same size as Loc (24 bytes)"
        );
        assert_eq!(std::mem::size_of::<ALoc>(), 24);
    }

    fn test_file_key() -> FileKey {
        FileKey::new(FileKeyInner::SourceFile("test.js".to_string()))
    }

    fn mk_loc_with_source(
        file: FileKey,
        start_line: i32,
        start_column: i32,
        end_line: i32,
        end_column: i32,
    ) -> Loc {
        Loc {
            source: Some(file),
            start: Position {
                line: start_line,
                column: start_column,
            },
            end: Position {
                line: end_line,
                column: end_column,
            },
        }
    }

    #[test]
    fn keyed_vs_concrete_whole_file() {
        let source = test_file_key();
        let concrete = ALoc::of_loc(mk_loc_with_source(source.dupe(), 0, 0, 0, 0));
        let keyed = aloc_representation_do_not_use::make_keyed(Some(source), 1);
        assert!(ALoc::compare(&concrete, &keyed) < std::cmp::Ordering::Equal);
        assert!(ALoc::compare(&keyed, &concrete) > std::cmp::Ordering::Equal);
    }

    #[test]
    #[should_panic(expected = "Unable to compare a keyed location with a concrete one")]
    fn keyed_vs_concrete() {
        let source = test_file_key();
        let concrete = ALoc::of_loc(mk_loc_with_source(source.dupe(), 1, 1, 1, 2));
        let keyed = aloc_representation_do_not_use::make_keyed(Some(source), 1);
        let _ = ALoc::compare(&concrete, &keyed);
    }

    // =========================================================================
    // ALocTable <-> PackedALocTable interaction tests
    // These tests exercise all the tricky conditions in packed_locs encoding
    // =========================================================================

    /// Helper to verify roundtrip: table -> packed -> table
    fn verify_roundtrip(locs: Vec<Loc>) {
        let file = test_file_key();
        let table = aloc_representation_do_not_use::make_table(file.dupe(), locs.clone());

        // Pack and unpack
        let packed = table.pack();
        let unpacked = ALocTable::unpack(file.dupe(), &packed);

        // Verify all locations match
        assert_eq!(
            unpacked.locs.len(),
            locs.len(),
            "Location count mismatch after roundtrip"
        );
        for (i, (original, restored)) in locs.iter().zip(unpacked.locs.iter()).enumerate() {
            assert_eq!(
                original.start, restored.start,
                "Start position mismatch at index {}",
                i
            );
            assert_eq!(
                original.end, restored.end,
                "End position mismatch at index {}",
                i
            );
        }

        // Verify double roundtrip produces same bytes
        let repacked = unpacked.pack();
        assert_eq!(
            &packed.0, &repacked.0,
            "Double roundtrip produced different bytes"
        );
    }

    // -------------------------------------------------------------------------
    // Empty and single-element tables
    // -------------------------------------------------------------------------

    #[test]
    fn empty_table_roundtrip() {
        verify_roundtrip(vec![]);
    }

    #[test]
    fn single_location_roundtrip() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file, 1, 5, 1, 10)]);
    }

    // -------------------------------------------------------------------------
    // Single-line locations (tags 0x00-0x7F)
    // -------------------------------------------------------------------------

    /// Tag 0x00-0x3F: single line, start_rline = 0, start_rcolumn < 0x40
    #[test]
    fn single_line_same_line_near_column() {
        let file = test_file_key();
        // Column 63 is the boundary (< 0x40 = 64)
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 63, 0, 64)]);
    }

    /// Tag 0x40: single line, start_rline = 0, start_rcolumn >= 0x40
    #[test]
    fn single_line_same_line_far_column() {
        let file = test_file_key();
        // Column 64 triggers the 0x40 tag path
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 64, 0, 65)]);
    }

    /// Tag 0x40-0x7E: single line, 0 < start_rline < 0x3F
    #[test]
    fn single_line_near_line_change() {
        let file = test_file_key();
        // Line 62 is at the boundary (< 0x3F = 63)
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 62, 1, 62, 3)]);
    }

    /// Tag 0x7F: single line, start_rline >= 0x3F
    #[test]
    fn single_line_far_line_change() {
        let file = test_file_key();
        // Line 63 triggers the 0x7F tag path
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 63, 1, 63, 3)]);
    }

    /// Single line with very large line number
    #[test]
    fn single_line_very_large_line() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 10000, 50, 10000, 100)]);
    }

    /// Single line with very large column
    #[test]
    fn single_line_very_large_column() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 1, 1000, 1, 2000)]);
    }

    // -------------------------------------------------------------------------
    // Multi-line locations (tags 0x80-0xFF)
    // -------------------------------------------------------------------------

    /// Tag 0x80-0xBF: multi line, start_rline = 0, start_rcolumn < 0x40
    #[test]
    fn multi_line_same_line_near_column() {
        let file = test_file_key();
        // Column 63 is boundary, spans to next line
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 63, 1, 2)]);
    }

    /// Tag 0xC0: multi line, start_rline = 0, start_rcolumn >= 0x40
    #[test]
    fn multi_line_same_line_far_column() {
        let file = test_file_key();
        // Column 64 triggers 0xC0 path
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 64, 1, 2)]);
    }

    /// Tag 0xC0-0xFE: multi line, 0 < start_rline < 0x3F
    #[test]
    fn multi_line_near_line_change() {
        let file = test_file_key();
        // Line 62, spanning multiple lines
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 62, 1, 64, 3)]);
    }

    /// Tag 0xFF: multi line, start_rline >= 0x3F
    #[test]
    fn multi_line_far_line_change() {
        let file = test_file_key();
        // Line 63 triggers the 0xFF tag path
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 63, 1, 64, 3)]);
    }

    /// Multi-line spanning many lines
    #[test]
    fn multi_line_large_span() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 100, 5, 500, 50)]);
    }

    // -------------------------------------------------------------------------
    // Multiple locations (relative encoding)
    // -------------------------------------------------------------------------

    /// Two consecutive locations on same line (tests relative column encoding)
    #[test]
    fn consecutive_same_line() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 1, 0, 1, 5),
            mk_loc_with_source(file.dupe(), 1, 10, 1, 15),
        ]);
    }

    /// Two locations with small relative offset
    #[test]
    fn small_relative_offset() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 1, 0, 1, 5),
            mk_loc_with_source(file.dupe(), 2, 0, 2, 5),
        ]);
    }

    /// Two locations with large relative offset (triggers LEB128)
    #[test]
    fn large_relative_offset() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 1, 0, 1, 5),
            mk_loc_with_source(file.dupe(), 1000, 0, 1000, 5),
        ]);
    }

    /// Locations crossing all tag boundaries in sequence
    #[test]
    fn all_tag_boundaries_sequence() {
        let file = test_file_key();
        verify_roundtrip(vec![
            // Tag 0x00-0x3F: single line, rline=0, rcolumn<0x40
            mk_loc_with_source(file.dupe(), 0, 10, 0, 20),
            // Tag 0x40-0x7E: single line, rline<0x3F (relative to prev)
            mk_loc_with_source(file.dupe(), 30, 5, 30, 15),
            // Tag 0x7F: single line, rline>=0x3F
            mk_loc_with_source(file.dupe(), 100, 0, 100, 10),
            // Tag 0x80-0xBF: multi line, rline=0, rcolumn<0x40
            mk_loc_with_source(file.dupe(), 100, 30, 102, 5),
            // Tag 0xC0-0xFE: multi line, rline<0x3F
            mk_loc_with_source(file.dupe(), 130, 0, 135, 10),
            // Tag 0xFF: multi line, rline>=0x3F
            mk_loc_with_source(file.dupe(), 200, 0, 210, 50),
        ]);
    }

    /// Many small locations (stress test relative encoding)
    #[test]
    fn many_small_locations() {
        let file = test_file_key();
        let locs: Vec<Loc> = (0..100)
            .map(|i| mk_loc_with_source(file.dupe(), i, i * 2, i, i * 2 + 5))
            .collect();
        verify_roundtrip(locs);
    }

    /// Mixed single-line and multi-line locations
    #[test]
    fn mixed_single_and_multi_line() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 1, 0, 1, 10), // single line
            mk_loc_with_source(file.dupe(), 2, 0, 5, 10), // multi line
            mk_loc_with_source(file.dupe(), 6, 0, 6, 20), // single line
            mk_loc_with_source(file.dupe(), 7, 0, 10, 5), // multi line
            mk_loc_with_source(file.dupe(), 11, 0, 11, 15), // single line
        ]);
    }

    // -------------------------------------------------------------------------
    // Edge cases and boundary conditions
    // -------------------------------------------------------------------------

    /// Location at position (0, 0) to (0, 0) - zero-length
    #[test]
    fn zero_length_location() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 0, 0, 0)]);
    }

    /// Location at column exactly 0x3F (63) - boundary
    #[test]
    fn column_at_0x3f_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 63, 0, 70)]);
    }

    /// Location at column exactly 0x40 (64) - boundary
    #[test]
    fn column_at_0x40_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 0, 64, 0, 70)]);
    }

    /// Location at line exactly 0x3E (62) - boundary
    #[test]
    fn line_at_0x3e_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 62, 0, 62, 10)]);
    }

    /// Location at line exactly 0x3F (63) - boundary
    #[test]
    fn line_at_0x3f_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![mk_loc_with_source(file.dupe(), 63, 0, 63, 10)]);
    }

    /// Relative column offset at boundary (prev_col + 63)
    #[test]
    fn relative_column_at_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 0, 0, 0, 5),
            mk_loc_with_source(file.dupe(), 0, 63, 0, 70), // rcolumn = 63 < 0x40
        ]);
    }

    /// Relative column offset just over boundary
    #[test]
    fn relative_column_over_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 0, 0, 0, 5),
            mk_loc_with_source(file.dupe(), 0, 64, 0, 70), // rcolumn = 64 >= 0x40
        ]);
    }

    /// Relative line offset at boundary
    #[test]
    fn relative_line_at_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 0, 0, 0, 5),
            mk_loc_with_source(file.dupe(), 62, 0, 62, 10), // rline = 62 < 0x3F
        ]);
    }

    /// Relative line offset just over boundary
    #[test]
    fn relative_line_over_boundary() {
        let file = test_file_key();
        verify_roundtrip(vec![
            mk_loc_with_source(file.dupe(), 0, 0, 0, 5),
            mk_loc_with_source(file.dupe(), 63, 0, 63, 10), // rline = 63 >= 0x3F
        ]);
    }

    // -------------------------------------------------------------------------
    // PackedALocTable API tests
    // -------------------------------------------------------------------------

    #[test]
    fn packed_size_increases_with_locations() {
        let file = test_file_key();

        let table1 = aloc_representation_do_not_use::make_table(file.dupe(), vec![]);
        let table2 = aloc_representation_do_not_use::make_table(
            file.dupe(),
            vec![mk_loc_with_source(file.dupe(), 1, 0, 1, 10)],
        );
        let table3 = aloc_representation_do_not_use::make_table(
            file.dupe(),
            vec![
                mk_loc_with_source(file.dupe(), 1, 0, 1, 10),
                mk_loc_with_source(file.dupe(), 2, 0, 2, 20),
                mk_loc_with_source(file.dupe(), 3, 0, 3, 30),
            ],
        );

        assert!(table1.pack().0.len() < table2.pack().0.len());
        assert!(table2.pack().0.len() < table3.pack().0.len());
    }
}
