/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A wrapper around ArcStr that implements Dupe for cheap cloning.
//!
//! ArcStr is a reference-counted string type with zero-cost literals and
//! cheap cloning via Arc.

use std::borrow::Borrow;
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;

use arcstr::ArcStr;
use dupe::Dupe;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use smol_str::SmolStr;

/// A wrapper around ArcStr that implements Dupe for cheap cloning.
///
/// ArcStr is a single-pointer-width reference-counted string. Cloning is
/// an atomic increment (8 bytes), much cheaper than SmolStr's 24-byte memcpy.
/// The smaller struct size (8 bytes vs 24) also improves cache utilization
/// and reduces memory usage for types that contain FlowSmolStr.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FlowSmolStr(ArcStr);

impl Dupe for FlowSmolStr {}

impl Debug for FlowSmolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0.as_str(), f)
    }
}

impl FlowSmolStr {
    pub fn new(s: impl AsRef<str>) -> Self {
        FlowSmolStr(ArcStr::from(s.as_ref()))
    }

    pub fn into_inner(self) -> SmolStr {
        SmolStr::from(self.0.as_str())
    }

    pub fn as_smol_str(&self) -> &FlowSmolStr {
        self
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates a new FlowSmolStr from a string slice.
    pub fn new_inline(s: &str) -> Self {
        FlowSmolStr(ArcStr::from(s))
    }
}

impl Deref for FlowSmolStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

impl AsRef<str> for FlowSmolStr {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl Borrow<str> for FlowSmolStr {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for FlowSmolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for FlowSmolStr {
    fn from(s: &str) -> Self {
        FlowSmolStr(ArcStr::from(s))
    }
}

impl From<&String> for FlowSmolStr {
    fn from(s: &String) -> Self {
        FlowSmolStr(ArcStr::from(s.as_str()))
    }
}

impl From<String> for FlowSmolStr {
    fn from(s: String) -> Self {
        FlowSmolStr(ArcStr::from(s.as_str()))
    }
}

impl From<SmolStr> for FlowSmolStr {
    fn from(s: SmolStr) -> Self {
        FlowSmolStr(ArcStr::from(s.as_str()))
    }
}

impl From<FlowSmolStr> for String {
    fn from(s: FlowSmolStr) -> Self {
        s.0.to_string()
    }
}

impl PartialEq<str> for FlowSmolStr {
    fn eq(&self, other: &str) -> bool {
        self.0.as_str() == other
    }
}

impl PartialEq<&str> for FlowSmolStr {
    fn eq(&self, other: &&str) -> bool {
        self.0.as_str() == *other
    }
}

impl PartialEq<String> for FlowSmolStr {
    fn eq(&self, other: &String) -> bool {
        self.0.as_str() == other.as_str()
    }
}

impl PartialEq<SmolStr> for FlowSmolStr {
    fn eq(&self, other: &SmolStr) -> bool {
        self.0.as_str() == other.as_str()
    }
}

impl Serialize for FlowSmolStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.as_str().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for FlowSmolStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer).map(|s| FlowSmolStr(ArcStr::from(s.as_str())))
    }
}
