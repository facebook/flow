/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::UnsafeCell;
use std::fmt;

use dupe::Dupe;

/// A context type, named independently of any borrow of it.
///
/// Values that outlive a single borrow of a context (lazy thunks in a type graph, an
/// environment, a cache) hold closures that are run with whichever borrow is current when they
/// are forced. Such closures take `&mut CX::Context<'a>` for every `'a`, so the structures holding
/// them never mention a borrow lifetime.
pub trait ContextFamily: 'static {
    type Context<'a>;
}

impl ContextFamily for () {
    type Context<'a> = ();
}

/// A closure run once with the context it is forced in.
pub type Thunk<CX, T> = Box<dyn for<'a> FnOnce(&mut <CX as ContextFamily>::Context<'a>) -> T>;

/// A lazily-evaluated value whose initializer closure computes a value of type `T`.
///
/// # Type Parameters
/// - `CX`: The family of the context the initializer is run with.
/// - `T`: The type of value being computed
pub struct Lazy<CX: ContextFamily, T> {
    inner: UnsafeCell<LazyInner<T, Thunk<CX, T>>>,
}

impl<CX: ContextFamily, T: fmt::Debug> fmt::Debug for Lazy<CX, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: We're only reading the discriminant/value, no mutation
        match unsafe { &*self.inner.get() } {
            LazyInner::Pending(_) => f.write_str("Lazy(<pending>)"),
            LazyInner::Forcing => f.write_str("Lazy(<forcing>)"),
            LazyInner::Forced(val) => f.debug_tuple("Lazy").field(val).finish(),
        }
    }
}

enum LazyInner<T, F> {
    Pending(F),
    Forcing,
    Forced(T),
}

impl<CX: ContextFamily, T> Lazy<CX, T> {
    /// Create a new lazy value with the given initializer function.
    #[inline(always)]
    pub fn new(f: impl for<'a> FnOnce(&mut CX::Context<'a>) -> T + 'static) -> Self {
        Self {
            inner: UnsafeCell::new(LazyInner::Pending(Box::new(f))),
        }
    }

    /// Create a lazy value that is already forced with the given value.
    #[inline(always)]
    pub fn new_forced(v: T) -> Self {
        Self {
            inner: UnsafeCell::new(LazyInner::Forced(v)),
        }
    }

    /// Get the forced value without computing it.
    ///
    /// # Panics
    /// Panics if the value has not yet been forced.
    #[inline(always)]
    pub fn as_already_forced(&self) -> &T {
        match unsafe { &*self.inner.get() } {
            LazyInner::Pending(_) => panic!("Called as_already_forced on unforced Lazy"),
            LazyInner::Forcing => panic!("Called as_already_forced while forcing"),
            LazyInner::Forced(v) => v,
        }
    }

    /// Check whether the value has been forced.
    #[inline(always)]
    pub fn is_forced(&self) -> bool {
        matches!(unsafe { &*self.inner.get() }, LazyInner::Forced(_))
    }

    /// Set the lazy value to an already-forced value, discarding any previous state.
    ///
    /// Uses interior mutability (UnsafeCell) to avoid requiring `&mut self`.
    ///
    /// # Safety (internal)
    /// Same safety model as `get_forced`: single-threaded, no overlapping references.
    #[inline(always)]
    pub fn set_forced(&self, v: T) {
        // SAFETY: Single-threaded access. No overlapping references.
        let inner = unsafe { &mut *self.inner.get() };
        *inner = LazyInner::Forced(v);
    }

    /// Reset the lazy value with a new closure, discarding any previous state.
    ///
    /// Uses interior mutability (UnsafeCell) to avoid requiring `&mut self`.
    ///
    /// # Safety (internal)
    /// Same safety model as `get_forced`: single-threaded, no overlapping references.
    #[inline(always)]
    pub fn reset(&self, f: impl for<'a> FnOnce(&mut CX::Context<'a>) -> T + 'static) {
        // SAFETY: Single-threaded access. No overlapping references.
        let inner = unsafe { &mut *self.inner.get() };
        *inner = LazyInner::Pending(Box::new(f));
    }

    /// Get the forced value, computing it with the given caller-provided function.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called.
    #[inline(always)]
    fn get_forced_with(&self, call: impl FnOnce(Thunk<CX, T>) -> T) -> &T {
        // Fast path: check if already forced using shared reference
        let inner = unsafe { &*self.inner.get() };
        if let LazyInner::Forced(val) = inner {
            return val;
        }

        let inner = unsafe { &mut *self.inner.get() };

        match inner {
            LazyInner::Forcing => panic!("Re-entrant forcing detected in Lazy::get_forced_with"),
            LazyInner::Pending(_) => {
                let LazyInner::Pending(f) = std::mem::replace(inner, LazyInner::Forcing) else {
                    unreachable!()
                };
                let val = call(f);
                *inner = LazyInner::Forced(val);
                let LazyInner::Forced(val) = inner else {
                    unreachable!()
                };
                val
            }
            LazyInner::Forced(val) => val,
        }
    }

    /// Get the forced value, computing it if necessary.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called (i.e., if forcing the value
    /// causes `get_forced` to be called again on the same `Lazy` instance).
    #[inline(always)]
    pub fn get_forced(&self, cx: &mut CX::Context<'_>) -> &T {
        self.get_forced_with(|f| f(cx))
    }

    /// Get the forced value as an owned copy, computing it if necessary.
    ///
    /// Requires `T: Dupe` so the value can be cheaply copied.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called.
    #[inline(always)]
    pub fn get_forced_into(&self, cx: &mut CX::Context<'_>) -> T
    where
        T: Dupe,
    {
        self.get_forced(cx).dupe()
    }
}

impl<CX: ContextFamily, T, E> Lazy<CX, Result<T, E>>
where
    E: Clone,
{
    /// Get the forced value, computing it if necessary, and propagate any error
    /// from the initializer closure to the caller.
    ///
    /// On the first force, the inner `Result<T, E>` is stored regardless of
    /// success or failure. Subsequent calls re-yield the stored Result by clone.
    /// This allows callers to `?`-propagate errors out of the lazy computation.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called.
    #[inline(always)]
    pub fn try_get_forced(&self, cx: &mut CX::Context<'_>) -> Result<&T, E> {
        let stored = self.get_forced_with(|f| f(cx));
        stored.as_ref().map_err(|e| e.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::rc::Rc;

    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Dupe)]
    struct TestValue(i32);

    enum StringCx {}

    impl ContextFamily for StringCx {
        type Context<'a> = String;
    }

    enum StrCx {}

    impl ContextFamily for StrCx {
        type Context<'a> = &'a str;
    }

    enum I32Cx {}

    impl ContextFamily for I32Cx {
        type Context<'a> = i32;
    }

    #[test]
    fn test_new_and_get_forced() {
        let lazy: Lazy<StringCx, _> = Lazy::new(|ctx: &mut String| {
            assert_eq!(ctx, "test_context");
            TestValue(42)
        });

        assert!(!lazy.is_forced());
        let value = lazy.get_forced(&mut "test_context".to_string());
        assert_eq!(*value, TestValue(42));
        assert!(lazy.is_forced());

        // Second call returns cached value
        let value2 = lazy.get_forced(&mut "different_context".to_string());
        assert_eq!(*value2, TestValue(42));
    }

    #[test]
    fn test_new_forced() {
        let lazy: Lazy<(), TestValue> = Lazy::new_forced(TestValue(100));

        assert!(lazy.is_forced());
        let value = lazy.as_already_forced();
        assert_eq!(*value, TestValue(100));
    }

    #[test]
    fn test_get_forced_into() {
        let lazy: Lazy<(), _> = Lazy::new(|_ctx: &mut ()| TestValue(55));

        let value = lazy.get_forced_into(&mut ());
        assert_eq!(value, TestValue(55));

        // Can call multiple times
        let value2 = lazy.get_forced_into(&mut ());
        assert_eq!(value2, TestValue(55));
    }

    #[test]
    fn test_as_already_forced() {
        let lazy: Lazy<(), _> = Lazy::new(|_ctx: &mut ()| TestValue(33));

        // Force it first
        lazy.get_forced(&mut ());

        // Now as_already_forced should work
        let value = lazy.as_already_forced();
        assert_eq!(*value, TestValue(33));
    }

    #[test]
    #[should_panic(expected = "Called as_already_forced on unforced Lazy")]
    fn test_as_already_forced_panics_if_not_forced() {
        let lazy: Lazy<(), TestValue> = Lazy::new(|_ctx: &mut ()| TestValue(1));
        let _ = lazy.as_already_forced();
    }

    #[test]
    #[should_panic(expected = "Re-entrant forcing detected")]
    fn test_reentrant_forcing_panics() {
        use std::cell::RefCell;

        let lazy_cell: Rc<RefCell<Option<Lazy<(), TestValue>>>> = Rc::new(RefCell::new(None));
        let lazy_cell_clone = lazy_cell.clone();

        let lazy: Lazy<(), TestValue> = Lazy::new(move |_ctx: &mut ()| {
            // Try to force the same lazy value recursively
            if let Some(ref inner_lazy) = *lazy_cell_clone.borrow() {
                // This should panic with re-entrant forcing
                inner_lazy.get_forced(&mut ());
            }
            TestValue(99)
        });

        *lazy_cell.borrow_mut() = Some(lazy);

        // This should trigger the re-entrant call and panic
        if let Some(ref the_lazy) = *lazy_cell.borrow() {
            the_lazy.get_forced(&mut ());
        }
    }

    #[test]
    fn test_is_forced() {
        let lazy: Lazy<I32Cx, _> = Lazy::new(|ctx: &mut i32| TestValue(*ctx));

        assert!(!lazy.is_forced());
        lazy.get_forced(&mut 123);
        assert!(lazy.is_forced());
    }

    #[test]
    fn test_closure_called_only_once() {
        let call_count = Rc::new(Cell::new(0));
        let call_count_clone = call_count.clone();

        let lazy: Lazy<StrCx, _> = Lazy::new(move |ctx: &mut &str| {
            call_count_clone.set(call_count_clone.get() + 1);
            TestValue(ctx.len() as i32)
        });

        assert_eq!(call_count.get(), 0);

        lazy.get_forced(&mut "test");
        assert_eq!(call_count.get(), 1);

        lazy.get_forced(&mut "another");
        assert_eq!(call_count.get(), 1); // Still 1, closure not called again
    }

    #[test]
    fn test_forced_with_any_borrow() {
        // The stored thunk names no borrow lifetime, so the lazy value can outlive the borrow
        // it is forced with.
        struct Borrowed<'a> {
            value: &'a i32,
        }

        enum BorrowedCx {}

        impl ContextFamily for BorrowedCx {
            type Context<'a> = Borrowed<'a>;
        }

        let lazy: Rc<Lazy<BorrowedCx, TestValue>> =
            Rc::new(Lazy::new(|c: &mut Borrowed<'_>| TestValue(*c.value)));
        {
            let value = 777;
            let result = lazy.get_forced(&mut Borrowed { value: &value });
            assert_eq!(*result, TestValue(777));
        }
        assert_eq!(*lazy.as_already_forced(), TestValue(777));
    }
}
