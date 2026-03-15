/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A wrapper around SmolStr that implements Dupe for cheap cloning.
//!
//! SmolStr is an efficient small string type that stores short strings inline.
//! This wrapper adds the Dupe trait, allowing cheap reference-counted copies
//! via `.dupe()` instead of `.clone()`.

use std::borrow::Borrow;
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;

use dupe::Dupe;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use smol_str::SmolStr;

/// A wrapper around SmolStr that implements Dupe for cheap cloning.
///
/// This type provides the same functionality as SmolStr but also implements
/// the Dupe trait, making it consistent with other types in the Flow codebase
/// that use `.dupe()` for cheap copies.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FlowSmolStr(SmolStr);

impl Dupe for FlowSmolStr {}

impl Debug for FlowSmolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl FlowSmolStr {
    pub fn new(s: impl Into<SmolStr>) -> Self {
        FlowSmolStr(s.into())
    }

    pub fn into_inner(self) -> SmolStr {
        self.0
    }

    pub fn as_smol_str(&self) -> &SmolStr {
        &self.0
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

    /// Creates a new FlowSmolStr from a static string slice.
    /// This is efficient for short strings that can be stored inline.
    pub fn new_inline(s: &str) -> Self {
        FlowSmolStr(SmolStr::new_inline(s))
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
        self.0.as_ref()
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
        FlowSmolStr(SmolStr::from(s))
    }
}

impl From<&String> for FlowSmolStr {
    fn from(s: &String) -> Self {
        FlowSmolStr(SmolStr::from(s.as_str()))
    }
}

impl From<String> for FlowSmolStr {
    fn from(s: String) -> Self {
        FlowSmolStr(SmolStr::from(s))
    }
}

impl From<SmolStr> for FlowSmolStr {
    fn from(s: SmolStr) -> Self {
        FlowSmolStr(s)
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
        self.0 == *other
    }
}

impl Serialize for FlowSmolStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for FlowSmolStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        SmolStr::deserialize(deserializer).map(FlowSmolStr)
    }
}
