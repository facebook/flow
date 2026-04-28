/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::UnsafeCell;
use std::fmt;
use std::marker::PhantomData;

use dupe::Dupe;

/// A lazily-evaluated value whose initializer closure computes a value of type `T`.
///
/// # Type Parameters
/// - `CX`: Context type marker (used for type-level association, e.g., `Context<'cx>`).
/// - `T`: The type of value being computed
/// - `F`: The closure type. For invariant context types, specify this explicitly
///   (e.g., `Box<dyn FnOnce(&Context<'cx>) -> T + 'cx>`).
pub struct Lazy<CX: ?Sized, T, F = Box<dyn FnOnce(&CX) -> T>> {
    inner: UnsafeCell<LazyInner<T, F>>,
    _phantom: PhantomData<fn() -> (T, CX)>,
}

impl<CX: ?Sized, T: fmt::Debug, F> fmt::Debug for Lazy<CX, T, F> {
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

impl<CX: ?Sized, T, F> Lazy<CX, T, F> {
    /// Create a new lazy value with the given initializer function.
    #[inline(always)]
    pub fn new(f: F) -> Self {
        Self {
            inner: UnsafeCell::new(LazyInner::Pending(f)),
            _phantom: PhantomData,
        }
    }

    /// Create a lazy value that is already forced with the given value.
    #[inline(always)]
    pub fn new_forced(v: T) -> Self {
        Self {
            inner: UnsafeCell::new(LazyInner::Forced(v)),
            _phantom: PhantomData,
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
    pub fn reset(&self, f: F) {
        // SAFETY: Single-threaded access. No overlapping references.
        let inner = unsafe { &mut *self.inner.get() };
        *inner = LazyInner::Pending(f);
    }
}

impl<CX: ?Sized, T, F> Lazy<CX, T, F> {
    /// Get the forced value, computing it with the given caller-provided function.
    ///
    /// This variant avoids HRTB issues with invariant types by letting the caller
    /// provide the forcing function explicitly.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called.
    #[inline(always)]
    fn get_forced_with(&self, call: impl FnOnce(F) -> T) -> &T {
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
}

impl<CX: ?Sized, T, F: FnOnce(&CX) -> T> Lazy<CX, T, F> {
    /// Get the forced value, computing it if necessary.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called (i.e., if forcing the value
    /// causes `get_forced` to be called again on the same `Lazy` instance).
    #[inline(always)]
    pub fn get_forced(&self, cx: &CX) -> &T {
        self.get_forced_with(|f| f(cx))
    }

    /// Get the forced value as an owned copy, computing it if necessary.
    ///
    /// Requires `T: Dupe` so the value can be cheaply copied.
    ///
    /// # Panics
    /// Panics if the closure is re-entrantly called.
    #[inline(always)]
    pub fn get_forced_into(&self, cx: &CX) -> T
    where
        T: Dupe,
    {
        self.get_forced(cx).dupe()
    }
}

impl<CX: ?Sized, T, E, F: FnOnce(&CX) -> Result<T, E>> Lazy<CX, Result<T, E>, F>
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
    pub fn try_get_forced(&self, cx: &CX) -> Result<&T, E> {
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

    #[test]
    fn test_new_and_get_forced() {
        let lazy = Lazy::new(|ctx: &String| {
            assert_eq!(ctx, "test_context");
            TestValue(42)
        });

        assert!(!lazy.is_forced());
        let value = lazy.get_forced(&"test_context".to_string());
        assert_eq!(*value, TestValue(42));
        assert!(lazy.is_forced());

        // Second call returns cached value
        let value2 = lazy.get_forced(&"different_context".to_string());
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
        let lazy = Lazy::new(|_ctx: &()| TestValue(55));

        let value = lazy.get_forced_into(&());
        assert_eq!(value, TestValue(55));

        // Can call multiple times
        let value2 = lazy.get_forced_into(&());
        assert_eq!(value2, TestValue(55));
    }

    #[test]
    fn test_as_already_forced() {
        let lazy = Lazy::new(|_ctx: &()| TestValue(33));

        // Force it first
        lazy.get_forced(&());

        // Now as_already_forced should work
        let value = lazy.as_already_forced();
        assert_eq!(*value, TestValue(33));
    }

    #[test]
    #[should_panic(expected = "Called as_already_forced on unforced Lazy")]
    fn test_as_already_forced_panics_if_not_forced() {
        let lazy: Lazy<(), TestValue, _> = Lazy::new(|_ctx: &()| TestValue(1));
        let _ = lazy.as_already_forced();
    }

    #[test]
    #[should_panic(expected = "Re-entrant forcing detected")]
    fn test_reentrant_forcing_panics() {
        use std::cell::RefCell;

        let lazy_cell: Rc<RefCell<Option<Lazy<(), TestValue>>>> = Rc::new(RefCell::new(None));
        let lazy_cell_clone = lazy_cell.clone();

        let lazy: Lazy<(), TestValue> = Lazy::new(Box::new(move |_ctx: &()| {
            // Try to force the same lazy value recursively
            if let Some(ref inner_lazy) = *lazy_cell_clone.borrow() {
                // This should panic with re-entrant forcing
                inner_lazy.get_forced(&());
            }
            TestValue(99)
        }));

        *lazy_cell.borrow_mut() = Some(lazy);

        // This should trigger the re-entrant call and panic
        if let Some(ref the_lazy) = *lazy_cell.borrow() {
            the_lazy.get_forced(&());
        }
    }

    #[test]
    fn test_is_forced() {
        let lazy = Lazy::new(|ctx: &i32| TestValue(*ctx));

        assert!(!lazy.is_forced());
        lazy.get_forced(&123);
        assert!(lazy.is_forced());
    }

    #[test]
    fn test_closure_called_only_once() {
        let call_count = Rc::new(Cell::new(0));
        let call_count_clone = call_count.clone();

        let lazy = Lazy::new(move |ctx: &str| {
            call_count_clone.set(call_count_clone.get() + 1);
            TestValue(ctx.len() as i32)
        });

        assert_eq!(call_count.get(), 0);

        lazy.get_forced("test");
        assert_eq!(call_count.get(), 1);

        lazy.get_forced("another");
        assert_eq!(call_count.get(), 1); // Still 1, closure not called again
    }

    #[test]
    fn test_with_reference_context() {
        struct Context {
            value: i32,
        }

        let ctx = Context { value: 777 };
        let lazy = Lazy::new(|c: &Context| TestValue(c.value));

        let result = lazy.get_forced(&ctx);
        assert_eq!(*result, TestValue(777));
    }

    #[test]
    fn test_boxed_closure() {
        let lazy: Lazy<str, TestValue> =
            Lazy::new(Box::new(|ctx: &str| TestValue(ctx.len() as i32)));

        let value = lazy.get_forced("hello");
        assert_eq!(*value, TestValue(5));
    }

    #[test]
    fn test_rc_lazy_no_lifetime() {
        // Test that Rc<Lazy<Context, bool>> works without lifetime parameters
        // — the key use case for LazyBool etc.
        struct Context {
            flag: bool,
        }

        let lazy: Rc<Lazy<Context, bool>> = Rc::new(Lazy::new(Box::new(|c: &Context| c.flag)));

        let ctx = Context { flag: true };
        let result = lazy.get_forced(&ctx);
        assert!(*result);
    }
}
