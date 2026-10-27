/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A pointer-sized immutable string that implements Dupe for cheap cloning.

use std::alloc;
use std::alloc::Layout;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::ptr;
use std::ptr::NonNull;
use std::slice;
use std::str;
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;

use dupe::Dupe;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use smol_str::SmolStr;

/// A pointer-sized immutable string that implements Dupe for cheap cloning.
///
/// The word holds one of three representations:
/// - inline: strings of up to 7 bytes live in the word itself;
/// - static: a `&'static str`, packed as a 48-bit address and its length;
/// - heap: the address of a reference-counted allocation, freed when the
///   last clone is dropped.
///
/// Only heap strings touch an atomic reference count on clone and drop.
/// Every string that fits inline is stored inline, so two inline strings are
/// equal exactly when their words are. Inline and static strings need a
/// 64-bit word; on narrower targets every string is a heap string.
pub struct FlowSmolStr(NonNull<u8>);

// SAFETY: inline and static strings are plain immutable data. Heap strings are
// shared like `Arc<str>`: their bytes are never written after construction, and
// their reference count is atomic.
unsafe impl Send for FlowSmolStr {}
// SAFETY: see the `Send` impl; no method mutates through `&self` except the
// atomic reference count.
unsafe impl Sync for FlowSmolStr {}

const PACKED: bool = cfg!(target_pointer_width = "64");

/// Set on inline words. The rest of the most significant byte holds the
/// length, and the other 7 bytes hold the string.
const INLINE_TAG: u64 = 1 << 63;
const INLINE_LEN_SHIFT: u32 = 56;
const INLINE_CAP: usize = 7;
/// Byte offset of the inline string within the word's memory.
const INLINE_DATA_OFFSET: usize = if cfg!(target_endian = "little") { 0 } else { 1 };

/// Static words keep their length (always more than `INLINE_CAP`) in bits
/// 48..63. Heap words are plain addresses, so their high 16 bits are zero.
const STATIC_LEN_SHIFT: u32 = 48;
const STATIC_MAX_LEN: usize = (1 << 15) - 1;
const ADDR_MASK: u64 = (1 << STATIC_LEN_SHIFT) - 1;

const MAX_REFCOUNT: usize = isize::MAX as usize;

/// Header of a heap string; the UTF-8 bytes follow it in the same allocation.
#[repr(C)]
struct HeapHeader {
    count: AtomicUsize,
    len: usize,
}

fn heap_layout(len: usize) -> Layout {
    Layout::from_size_align(size_of::<HeapHeader>() + len, align_of::<HeapHeader>())
        .expect("FlowSmolStr: string too long")
}

impl Dupe for FlowSmolStr {}

impl Debug for FlowSmolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl FlowSmolStr {
    pub fn new(s: impl AsRef<str>) -> Self {
        Self::from_str(s.as_ref())
    }

    pub fn into_inner(self) -> SmolStr {
        SmolStr::from(self.as_str())
    }

    pub fn as_smol_str(&self) -> &FlowSmolStr {
        self
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        let word = self.word();
        let (data, len) = if word & INLINE_TAG != 0 {
            let data = ptr::from_ref(self)
                .cast::<u8>()
                .wrapping_add(INLINE_DATA_OFFSET);
            (data, (word >> INLINE_LEN_SHIFT) as usize & 0x7f)
        } else if word & !ADDR_MASK != 0 {
            let data = self.0.as_ptr().map_addr(|addr| addr & ADDR_MASK as usize);
            (data.cast_const(), (word >> STATIC_LEN_SHIFT) as usize)
        } else {
            let header = self.0.cast::<HeapHeader>().as_ptr();
            // SAFETY: a heap word is the address of a live heap string, which
            // `self` keeps alive.
            (header.wrapping_add(1).cast_const().cast::<u8>(), unsafe {
                (*header).len
            })
        };
        // SAFETY: an inline word holds `len` bytes of a valid string inside
        // `self`, a static word holds the address and length of a
        // `&'static str`, and a heap string holds `len` bytes of a valid string
        // right after its header.
        unsafe { str::from_utf8_unchecked(slice::from_raw_parts(data, len)) }
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    /// Creates a new FlowSmolStr from a `&'static str` without allocating.
    pub fn new_inline(s: &'static str) -> Self {
        let addr = s.as_ptr().addr() as u64;
        if !PACKED || s.len() <= INLINE_CAP || s.len() > STATIC_MAX_LEN || addr & !ADDR_MASK != 0 {
            return Self::from_str(s);
        }
        let len_bits = ((s.len() as u64) << STATIC_LEN_SHIFT) as usize;
        let data = s.as_ptr().cast_mut().map_addr(|addr| addr | len_bits);
        FlowSmolStr(NonNull::new(data).expect("should be nonzero: the length bits are set"))
    }

    #[inline]
    fn word(&self) -> u64 {
        self.0.addr().get() as u64
    }

    fn from_str(s: &str) -> Self {
        if PACKED && s.len() <= INLINE_CAP {
            Self::inline(s)
        } else {
            Self::heap(s)
        }
    }

    fn inline(s: &str) -> Self {
        let mut bytes = [0u8; 8];
        bytes[INLINE_DATA_OFFSET..INLINE_DATA_OFFSET + s.len()].copy_from_slice(s.as_bytes());
        let word = u64::from_ne_bytes(bytes) | INLINE_TAG | (s.len() as u64) << INLINE_LEN_SHIFT;
        let data = ptr::without_provenance_mut(word as usize);
        FlowSmolStr(NonNull::new(data).expect("should be nonzero: INLINE_TAG is set"))
    }

    fn heap(s: &str) -> Self {
        let layout = heap_layout(s.len());
        // SAFETY: the layout has a nonzero size.
        let header = unsafe { alloc::alloc(layout) }.cast::<HeapHeader>();
        let Some(header) = NonNull::new(header) else {
            alloc::handle_alloc_error(layout);
        };
        // SAFETY: the allocation is aligned for `HeapHeader` and has room for
        // the header followed by `s.len()` bytes.
        unsafe {
            header.write(HeapHeader {
                count: AtomicUsize::new(1),
                len: s.len(),
            });
            ptr::copy_nonoverlapping(s.as_ptr(), header.add(1).cast::<u8>().as_ptr(), s.len());
        }
        if (header.addr().get() as u64) & !ADDR_MASK != 0 {
            // SAFETY: the allocation was made above with `layout` and is not shared.
            unsafe { alloc::dealloc(header.cast::<u8>().as_ptr(), layout) };
            panic!("FlowSmolStr: heap address does not fit in 48 bits");
        }
        FlowSmolStr(header.cast::<u8>())
    }

    #[inline]
    fn heap_header(&self) -> Option<NonNull<HeapHeader>> {
        if self.word() & !ADDR_MASK == 0 {
            Some(self.0.cast::<HeapHeader>())
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// `header` must be a heap string whose last reference was just released.
    #[cold]
    #[inline(never)]
    unsafe fn drop_heap(header: NonNull<HeapHeader>) {
        atomic::fence(atomic::Ordering::Acquire);
        // SAFETY: the caller released the last reference, so the allocation is
        // live and nothing else can observe it.
        let len = unsafe { header.as_ref().len };
        // SAFETY: the allocation was made by `heap` with the same layout.
        unsafe { alloc::dealloc(header.cast::<u8>().as_ptr(), heap_layout(len)) };
    }
}

impl Clone for FlowSmolStr {
    #[inline]
    fn clone(&self) -> Self {
        if let Some(header) = self.heap_header() {
            // SAFETY: `self` keeps the heap string alive.
            let old = unsafe { header.as_ref() }
                .count
                .fetch_add(1, atomic::Ordering::Relaxed);
            if old > MAX_REFCOUNT {
                std::process::abort();
            }
        }
        FlowSmolStr(self.0)
    }
}

impl Drop for FlowSmolStr {
    #[inline]
    fn drop(&mut self) {
        if let Some(header) = self.heap_header() {
            // SAFETY: `self` holds a reference until the decrement.
            let old = unsafe { header.as_ref() }
                .count
                .fetch_sub(1, atomic::Ordering::Release);
            if old == 1 {
                // SAFETY: `self` held the last reference.
                unsafe { Self::drop_heap(header) };
            }
        }
    }
}

impl Default for FlowSmolStr {
    fn default() -> Self {
        Self::from_str("")
    }
}

impl PartialEq for FlowSmolStr {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
            || ((self.word() | other.word()) & INLINE_TAG == 0 && self.as_str() == other.as_str())
    }
}

impl Eq for FlowSmolStr {}

impl PartialOrd for FlowSmolStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FlowSmolStr {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        if self.0 == other.0 {
            Ordering::Equal
        } else {
            self.as_str().cmp(other.as_str())
        }
    }
}

impl Hash for FlowSmolStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl Deref for FlowSmolStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<str> for FlowSmolStr {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for FlowSmolStr {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for FlowSmolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&str> for FlowSmolStr {
    fn from(s: &str) -> Self {
        FlowSmolStr::from_str(s)
    }
}

impl From<&String> for FlowSmolStr {
    fn from(s: &String) -> Self {
        FlowSmolStr::from_str(s.as_str())
    }
}

impl From<String> for FlowSmolStr {
    fn from(s: String) -> Self {
        FlowSmolStr::from_str(s.as_str())
    }
}

impl From<SmolStr> for FlowSmolStr {
    fn from(s: SmolStr) -> Self {
        FlowSmolStr::from_str(s.as_str())
    }
}

impl From<FlowSmolStr> for String {
    fn from(s: FlowSmolStr) -> Self {
        s.as_str().to_owned()
    }
}

impl PartialEq<str> for FlowSmolStr {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for FlowSmolStr {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for FlowSmolStr {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<SmolStr> for FlowSmolStr {
    fn eq(&self, other: &SmolStr) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Serialize for FlowSmolStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for FlowSmolStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        String::deserialize(deserializer).map(FlowSmolStr::from)
    }
}
