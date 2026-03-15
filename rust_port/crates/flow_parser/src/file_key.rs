/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;

#[derive(Debug, Clone)]
pub enum FileKeyInner {
    LibFile(String),
    SourceFile(String),
    JsonFile(String),
    /// A resource that might get required, like .css, .jpg, etc.
    /// We don't parse these, just check that they exist.
    ResourceFile(String),
}

// libs, then source and json files at the same priority since JSON files are
// basically source files. We don't actually read resource files so they come
// last
fn order_of_inner(inner: &FileKeyInner) -> i32 {
    match inner {
        FileKeyInner::LibFile(_) => 1,
        FileKeyInner::SourceFile(_) => 2,
        FileKeyInner::JsonFile(_) => 2,
        FileKeyInner::ResourceFile(_) => 3,
    }
}

fn inner_as_str(inner: &FileKeyInner) -> &str {
    match inner {
        FileKeyInner::LibFile(x) => x.as_str(),
        FileKeyInner::SourceFile(x) => x.as_str(),
        FileKeyInner::JsonFile(x) => x.as_str(),
        FileKeyInner::ResourceFile(x) => x.as_str(),
    }
}

impl PartialEq for FileKeyInner {
    fn eq(&self, other: &Self) -> bool {
        order_of_inner(self) == order_of_inner(other) && inner_as_str(self) == inner_as_str(other)
    }
}

impl Eq for FileKeyInner {}

impl Hash for FileKeyInner {
    fn hash<H: Hasher>(&self, state: &mut H) {
        order_of_inner(self).hash(state);
        inner_as_str(self).hash(state);
    }
}

#[derive(Debug, Clone, Dupe)]
pub struct FileKey {
    inner: Arc<FileKeyInner>,
}

impl PartialEq for FileKey {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner) || self.inner.as_ref() == other.inner.as_ref()
    }
}

impl Eq for FileKey {}

impl Hash for FileKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.as_ref().hash(state);
    }
}

fn order_of_filename(file_key: &FileKey) -> i32 {
    order_of_inner(file_key.inner.as_ref())
}

impl PartialOrd for FileKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if Arc::ptr_eq(&self.inner, &other.inner) {
            return std::cmp::Ordering::Equal;
        }
        let k = order_of_filename(self).cmp(&order_of_filename(other));
        if k != std::cmp::Ordering::Equal {
            k
        } else {
            self.as_str().cmp(other.as_str())
        }
    }
}

impl FileKey {
    pub fn new(inner: FileKeyInner) -> Self {
        Self {
            inner: Arc::new(inner),
        }
    }

    pub fn as_str(&self) -> &str {
        match self.inner.as_ref() {
            FileKeyInner::LibFile(x) => x.as_str(),
            FileKeyInner::SourceFile(x) => x.as_str(),
            FileKeyInner::JsonFile(x) => x.as_str(),
            FileKeyInner::ResourceFile(x) => x.as_str(),
        }
    }

    pub fn inner(&self) -> &FileKeyInner {
        self.inner.as_ref()
    }

    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf::from(self.as_str())
    }

    pub fn is_lib_file(&self) -> bool {
        match self.inner.as_ref() {
            FileKeyInner::LibFile(_) => true,
            FileKeyInner::SourceFile(_) => false,
            FileKeyInner::JsonFile(_) => false,
            FileKeyInner::ResourceFile(_) => false,
        }
    }

    pub fn map<F: FnOnce(&str) -> String>(&self, f: F) -> FileKey {
        let new_inner = match self.inner.as_ref() {
            FileKeyInner::LibFile(filename) => FileKeyInner::LibFile(f(filename)),
            FileKeyInner::SourceFile(filename) => FileKeyInner::SourceFile(f(filename)),
            FileKeyInner::JsonFile(filename) => FileKeyInner::JsonFile(f(filename)),
            FileKeyInner::ResourceFile(filename) => FileKeyInner::ResourceFile(f(filename)),
        };
        FileKey::new(new_inner)
    }

    pub fn exists<F: FnOnce(&str) -> bool>(&self, f: F) -> bool {
        match self.inner.as_ref() {
            FileKeyInner::LibFile(intern)
            | FileKeyInner::SourceFile(intern)
            | FileKeyInner::JsonFile(intern)
            | FileKeyInner::ResourceFile(intern) => f(intern.as_str()),
        }
    }

    pub fn check_suffix(&self, suffix: &str) -> bool {
        self.exists(|filename| filename.ends_with(suffix))
    }

    pub fn chop_suffix(&self, suffix: &str) -> FileKey {
        self.map(|filename| filename.strip_suffix(suffix).expect(filename).to_string())
    }

    pub fn with_suffix(&self, suffix: &str) -> FileKey {
        self.map(|filename| format!("{}{}", filename, suffix))
    }
}
