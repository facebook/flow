/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Wrapper around `rpds::List` — an OCaml-style cons list.

use std::ops::Deref;

use dupe::Dupe;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FlowOcamlList<T>(rpds::List<T>);

impl<T> Default for FlowOcamlList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Dupe for FlowOcamlList<T> {}

impl<T> FlowOcamlList<T> {
    pub fn new() -> Self {
        FlowOcamlList(rpds::List::new())
    }

    pub fn unit(value: T) -> Self {
        let mut l = rpds::List::new();
        l.push_front_mut(value);
        FlowOcamlList(l)
    }

    pub fn into_inner(self) -> rpds::List<T> {
        self.0
    }

    pub fn as_inner(&self) -> &rpds::List<T> {
        &self.0
    }

    pub fn push_front(&mut self, value: T) {
        self.0.push_front_mut(value);
    }

    pub fn drop_first(&mut self) -> bool {
        self.0.drop_first_mut()
    }

    pub fn reverse(&mut self) {
        self.0.reverse_mut();
    }
}

impl<T> Deref for FlowOcamlList<T> {
    type Target = rpds::List<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> From<rpds::List<T>> for FlowOcamlList<T> {
    fn from(l: rpds::List<T>) -> Self {
        FlowOcamlList(l)
    }
}

impl<T> From<FlowOcamlList<T>> for rpds::List<T> {
    fn from(l: FlowOcamlList<T>) -> Self {
        l.0
    }
}

impl<T> FromIterator<T> for FlowOcamlList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        FlowOcamlList(rpds::List::from_iter(iter))
    }
}

impl<'a, T> IntoIterator for &'a FlowOcamlList<T> {
    type Item = &'a T;
    type IntoIter = <&'a rpds::List<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}
