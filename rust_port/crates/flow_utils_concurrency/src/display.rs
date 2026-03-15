/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Utilities for formatting output.
//!
//! Implementation note: The first attempt at these utilities used types, rather than functions,
//! but then type errors turn into nasty link instantiation overflow errors which are impossible to debug.

use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use std::sync::Arc;

use itertools::Either;

pub fn append<A, B>(a: A, b: B) -> impl Iterator<Item = Either<A::Item, B::Item>>
where
    A: IntoIterator,
    B: IntoIterator,
{
    a.into_iter()
        .map(Either::Left)
        .chain(b.into_iter().map(Either::Right))
}

pub fn commas_iter<F, A>(a: F) -> impl Display
where
    F: Fn() -> A,
    A: IntoIterator<Item: Display>,
{
    intersperse_iter(", ", a)
}

pub fn intersperse_iter<F, A, S>(separator: S, a: F) -> impl Display
where
    F: Fn() -> A,
    A: IntoIterator<Item: Display>,
    S: Display,
{
    struct Intersperse<F, S>(F, S);
    impl<F, A, S> Display for Intersperse<F, S>
    where
        F: Fn() -> A,
        A: IntoIterator<Item: Display>,
        S: Display,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for (i, item) in (self.0)().into_iter().enumerate() {
                if i != 0 {
                    Display::fmt(&self.1, f)?;
                }
                Display::fmt(&item, f)?;
            }
            Ok(())
        }
    }
    Intersperse(a, separator)
}

pub struct Fmt<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result>(pub F);

impl<F> Display for Fmt<F>
where
    F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

/// Like `Display`, but allows passing some additional context.
pub trait DisplayWith<Ctx: ?Sized> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result;
}

pub trait DisplayWithCtx<Ctx: ?Sized>: DisplayWith<Ctx> {
    fn display_with<'a>(&'a self, ctx: &'a Ctx) -> impl Display + 'a {
        struct X<'a, T: ?Sized, Ctx: ?Sized>(&'a T, &'a Ctx);
        impl<'a, Ctx: ?Sized, T: DisplayWith<Ctx> + ?Sized> Display for X<'a, T, Ctx> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f, self.1)
            }
        }
        X(self, ctx)
    }
}

impl<T: ?Sized, Ctx: ?Sized> DisplayWithCtx<Ctx> for T where T: DisplayWith<Ctx> {}

// General wrappers

impl<Ctx: ?Sized, T: DisplayWith<Ctx>> DisplayWith<Ctx> for &T {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayWith::<Ctx>::fmt(*self, f, ctx)
    }
}

impl<Ctx: ?Sized, T: DisplayWith<Ctx>> DisplayWith<Ctx> for &mut T {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayWith::<Ctx>::fmt(*self, f, ctx)
    }
}

impl<Ctx: ?Sized, T: DisplayWith<Ctx>> DisplayWith<Ctx> for Box<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayWith::<Ctx>::fmt(&**self, f, ctx)
    }
}

impl<Ctx: ?Sized, T: DisplayWith<Ctx>> DisplayWith<Ctx> for Arc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayWith::<Ctx>::fmt(&**self, f, ctx)
    }
}

impl<Ctx: ?Sized, T: DisplayWith<Ctx>> DisplayWith<Ctx> for Rc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Ctx) -> fmt::Result {
        DisplayWith::<Ctx>::fmt(&**self, f, ctx)
    }
}

/// Print a number using commas between thousands.
pub fn number_thousands(x: usize) -> impl Display {
    // Taken from https://stackoverflow.com/a/67834588
    x.to_string()
        .as_bytes()
        .rchunks(3)
        .rev()
        .map(std::str::from_utf8)
        .collect::<Result<Vec<&str>, _>>()
        .unwrap()
        .join(",")
}

/// Format the number and descriptor word with the right pluralization.
/// E.g., count(2, "cat") -> "2 cats". Pluralizes by tacking on an "s".
pub fn count(n: usize, descriptor: &str) -> String {
    format!(
        "{} {descriptor}{}",
        number_thousands(n),
        if n == 1 { "" } else { "s" }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_commas() {
        assert_eq!(
            commas_iter(|| append([1, 2], ["three"])).to_string(),
            "1, 2, three"
        );
        assert_eq!(
            commas_iter(|| [1, 2].iter().map(|x: &i32| -x)).to_string(),
            "-1, -2"
        );
    }

    #[test]
    fn test_fmt() {
        assert_eq!(Fmt(|f| write!(f, "hello")).to_string(), "hello");
    }

    #[test]
    fn test_number_thousands() {
        assert_eq!(number_thousands(0).to_string(), "0");
        assert_eq!(number_thousands(1).to_string(), "1");
        assert_eq!(number_thousands(10).to_string(), "10");
        assert_eq!(number_thousands(100).to_string(), "100");
        assert_eq!(number_thousands(1000).to_string(), "1,000");
        assert_eq!(number_thousands(123456).to_string(), "123,456");
        assert_eq!(number_thousands(1040030009).to_string(), "1,040,030,009");
    }
}
