/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocId;
use flow_common::reason;

/// In order to minimize the frequency with which we unnecessarily compare
/// equivalent structures, we assign structures created at the top level of a
/// source program an id of their location instead of an int. This way, if we see
/// the structure twice between the type-sig and check phases, we consider them
/// equal just by looking at their ids (see equal_id).
///
/// Note that we still store both versions of the type (distinguished through the
/// value of the 'type_sig' field), since the types generated in each phase do not
/// have identical properties. One example is that type-sig produces FullyResolved
/// types exclusively, whereas check can create types that are unresolved. It is
/// important to keep these types separate and not use them interchangeably, or we
/// risk breaking the invariant that the FullyResolved constructor deeply contains
/// other FullyResolved types. (Regression tests under tests/recursive_defs)
///
/// Source ids with the same location are considered equal w.r.t. type checking
/// regardless of the value of type_sig, which means regardless of whether they
/// were generated during type-sig of check phase.
#[derive(Debug, Clone, Eq, Dupe)]
pub struct Id(Arc<IdInner>);

#[derive(Debug, Clone, Eq, PartialEq)]
enum IdInner {
    Source { loc: ALoc, type_sig: bool },
    Generated(i32),
}

impl Ord for Id {
    fn cmp(&self, other: &Self) -> Ordering {
        if Arc::ptr_eq(&self.0, &other.0) {
            return Ordering::Equal;
        }
        match (self.0.as_ref(), other.0.as_ref()) {
            (
                IdInner::Source {
                    loc: loc1,
                    type_sig: b1,
                },
                IdInner::Source {
                    loc: loc2,
                    type_sig: b2,
                },
            ) => match b1.cmp(b2) {
                Ordering::Equal => loc1.quick_compare(loc2),
                other => other,
            },
            (IdInner::Generated(a), IdInner::Generated(b)) => a.cmp(b),
            (IdInner::Source { .. }, IdInner::Generated(_)) => Ordering::Less,
            (IdInner::Generated(_), IdInner::Source { .. }) => Ordering::Greater,
        }
    }
}

impl PartialOrd for Id {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
            || match (self.0.as_ref(), other.0.as_ref()) {
                (IdInner::Source { loc: loc1, .. }, IdInner::Source { loc: loc2, .. }) => {
                    loc1.quick_compare(loc2) == Ordering::Equal
                }
                (IdInner::Generated(a), IdInner::Generated(b)) => a == b,
                _ => false,
            }
    }
}

impl std::hash::Hash for Id {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self.0.as_ref() {
            // NOTE: type_sig is intentionally excluded from Hash to be consistent with PartialEq,
            // which considers Source ids equal if they have the same loc regardless of type_sig.
            IdInner::Source { loc, type_sig: _ } => {
                0u8.hash(state);
                loc.hash(state);
            }
            IdInner::Generated(id) => {
                1u8.hash(state);
                id.hash(state);
            }
        }
    }
}

impl Id {
    pub fn generate_id() -> Self {
        let id = reason::mk_id() as i32;
        Self(Arc::new(IdInner::Generated(id)))
    }

    pub fn of_aloc_id(type_sig: bool, aloc_id: ALocId) -> Self {
        Self(Arc::new(IdInner::Source {
            loc: aloc_id.0,
            type_sig,
        }))
    }

    pub fn from_type_sig(&self) -> bool {
        match self.0.as_ref() {
            IdInner::Source { type_sig, .. } => *type_sig,
            IdInner::Generated(_) => false,
        }
    }

    pub fn debug_string(&self) -> String {
        match self.0.as_ref() {
            IdInner::Generated(id) => id.to_string(),
            IdInner::Source { type_sig, loc } => {
                format!(
                    "{} (type_sig: {})",
                    reason::string_of_aloc(None, loc),
                    type_sig
                )
            }
        }
    }

    pub fn stable_string(&self) -> String {
        match self.0.as_ref() {
            IdInner::Generated(id) => id.to_string(),
            IdInner::Source { type_sig, loc } => {
                format!("{} (type_sig: {})", loc.to_string_no_source(), type_sig)
            }
        }
    }
}
