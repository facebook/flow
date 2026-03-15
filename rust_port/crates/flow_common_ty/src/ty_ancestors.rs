/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_common::reason::Name;

use crate::ty_symbol::Symbol;

pub trait TyIterBase<Env, L> {
    fn on_string(&mut self, _env: &Env, _s: &str) {}

    fn on_name(&mut self, _env: &Env, _name: &Name) {}

    fn on_bool(&mut self, _env: &Env, _b: bool) {}

    fn on_int(&mut self, _env: &Env, _i: i64) {}

    fn on_symbol(&mut self, _env: &Env, _sym: &Symbol<L>) {}

    fn on_aloc(&mut self, _env: &Env, _loc: &ALoc) {}

    fn on_option<T, B, F>(&mut self, f: F, env: &Env, opt: &Option<T>) -> Option<B>
    where
        F: Fn(&mut Self, &Env, &T) -> B,
    {
        opt.as_ref().map(|x| f(self, env, x))
    }

    fn on_list<T, F>(&mut self, f: F, env: &Env, list: &[T])
    where
        F: Fn(&mut Self, &Env, &T),
    {
        for item in list {
            f(self, env, item);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructuralMismatch {
    Mismatch,
    Difference(i32),
}

impl StructuralMismatch {
    pub fn assert0(n: i32) -> Result<(), StructuralMismatch> {
        if n == 0 {
            Ok(())
        } else {
            Err(StructuralMismatch::Difference(n))
        }
    }

    pub fn fail_gen<T>(tag_of: impl Fn(&T) -> i32, x: &T, y: &T) -> Result<(), StructuralMismatch> {
        Self::assert0(tag_of(x) - tag_of(y))
    }
}

pub trait TyIter2Base<Env, L> {
    fn on_string(&mut self, env: &Env, s1: &str, s2: &str) -> Result<(), StructuralMismatch> {
        /* In order to sort integer literals we try to parse all strings as integers */
        match s1.parse::<i64>() {
            Ok(xi) => match s2.parse::<i64>() {
                /* If both parse as integers then we compare them as integers */
                Ok(yi) => self.on_int(env, xi, yi),
                /* If xor parses as an integer then that one is "less than" the other */
                Err(_) => Err(StructuralMismatch::Difference(-1)),
            },
            Err(_) => match s2.parse::<i64>() {
                Ok(_) => Err(StructuralMismatch::Difference(1)),
                /* If neither parse as integers then we compare them as strings */
                Err(_) => StructuralMismatch::assert0(s1.cmp(s2) as i32),
            },
        }
    }

    fn on_name(&mut self, env: &Env, name1: &Name, name2: &Name) -> Result<(), StructuralMismatch> {
        self.on_string(env, name1.as_str(), name2.as_str())
    }

    fn on_bool(&mut self, _env: &Env, b1: bool, b2: bool) -> Result<(), StructuralMismatch> {
        StructuralMismatch::assert0(b1.cmp(&b2) as i32)
    }

    fn on_int(&mut self, _env: &Env, i1: i64, i2: i64) -> Result<(), StructuralMismatch> {
        StructuralMismatch::assert0((i1 - i2).signum() as i32)
    }

    fn on_symbol(
        &mut self,
        _env: &Env,
        sym1: &Symbol<L>,
        sym2: &Symbol<L>,
    ) -> Result<(), StructuralMismatch> {
        StructuralMismatch::assert0(sym1.sym_name.cmp(&sym2.sym_name) as i32)
    }

    fn on_aloc(&mut self, _env: &Env, loc1: &ALoc, loc2: &ALoc) -> Result<(), StructuralMismatch> {
        StructuralMismatch::assert0(loc1.cmp(loc2) as i32)
    }

    fn fail_option<T>(
        &mut self,
        _env: &Env,
        opt1: &Option<T>,
        _opt2: &Option<T>,
    ) -> Result<(), StructuralMismatch> {
        match opt1 {
            None => Err(StructuralMismatch::Difference(-1)),
            Some(_) => Err(StructuralMismatch::Difference(1)),
        }
    }

    fn on_option<T, F>(
        &mut self,
        f: F,
        env: &Env,
        opt1: &Option<T>,
        opt2: &Option<T>,
    ) -> Result<(), StructuralMismatch>
    where
        F: Fn(&mut Self, &Env, &T, &T) -> Result<(), StructuralMismatch>,
    {
        match (opt1, opt2) {
            (Some(x), Some(y)) => f(self, env, x, y),
            (None, None) => Ok(()),
            _ => self.fail_option(env, opt1, opt2),
        }
    }

    fn fail_list<T>(
        &mut self,
        _env: &Env,
        list1: &[T],
        _list2: &[T],
    ) -> Result<(), StructuralMismatch> {
        if list1.is_empty() {
            Err(StructuralMismatch::Difference(-1))
        } else {
            Err(StructuralMismatch::Difference(1))
        }
    }

    fn on_list<T, F>(
        &mut self,
        f: F,
        env: &Env,
        list1: &[T],
        list2: &[T],
    ) -> Result<(), StructuralMismatch>
    where
        F: Fn(&mut Self, &Env, &T, &T) -> Result<(), StructuralMismatch>,
    {
        if list1.len() != list2.len() {
            return self.fail_list(env, list1, list2);
        }
        for (item1, item2) in list1.iter().zip(list2.iter()) {
            f(self, env, item1, item2)?;
        }
        Ok(())
    }
}

pub trait TyMapBase<Env, L> {
    fn on_string(&mut self, _env: &Env, s: String) -> String {
        s
    }

    fn on_name(&mut self, _env: &Env, name: Name) -> Name {
        name
    }

    fn on_bool(&mut self, _env: &Env, b: bool) -> bool {
        b
    }

    fn on_int(&mut self, _env: &Env, i: i64) -> i64 {
        i
    }

    fn on_symbol(&mut self, _env: &Env, sym: Symbol<L>) -> Symbol<L> {
        sym
    }

    fn on_aloc(&mut self, _env: &Env, loc: ALoc) -> ALoc {
        loc
    }

    fn on_list<T: Clone, F>(&mut self, f: F, env: &Env, list: &[T]) -> Vec<T>
    where
        F: Fn(&mut Self, &Env, T) -> T,
    {
        list.iter().map(|x| f(self, env, x.clone())).collect()
    }

    fn on_option<T, F>(&mut self, f: F, env: &Env, opt: Option<T>) -> Option<T>
    where
        F: Fn(&mut Self, &Env, T) -> T,
    {
        opt.map(|x| f(self, env, x))
    }
}

pub trait TyEndoBase<Env, L> {
    fn on_string(&mut self, _env: &Env, s: String) -> String {
        s
    }

    fn on_name(&mut self, _env: &Env, name: Name) -> Name {
        name
    }

    fn on_bool(&mut self, _env: &Env, b: bool) -> bool {
        b
    }

    fn on_int(&mut self, _env: &Env, i: i64) -> i64 {
        i
    }

    fn on_symbol(&mut self, _env: &Env, sym: Symbol<L>) -> Symbol<L> {
        sym
    }

    fn on_aloc(&mut self, _env: &Env, loc: ALoc) -> ALoc {
        loc
    }

    /* Copied from
     * https://github.com/facebook/hhvm/blob/master/hphp/hack/src/ast/ast_defs_visitors_ancestors.ml
     */
    fn on_list<T: Clone, F>(&mut self, f: F, env: &Env, list: &[T]) -> Vec<T>
    where
        F: Fn(&mut Self, &Env, T) -> T,
    {
        list.iter().map(|v| f(self, env, v.clone())).collect()
    }

    fn on_option<T, F>(&mut self, f: F, env: &Env, opt: Option<T>) -> Option<T>
    where
        F: Fn(&mut Self, &Env, T) -> T,
    {
        opt.map(|x| f(self, env, x))
    }
}

pub trait Monoid {
    fn zero() -> Self;

    fn plus(a: Self, b: Self) -> Self;
}

pub trait TyReduceBase<Env, L> {
    type Acc: Monoid;

    fn on_string(&mut self, _env: &Env, _s: &str) -> Self::Acc {
        Self::Acc::zero()
    }

    fn on_name(&mut self, _env: &Env, _name: &Name) -> Self::Acc {
        Self::Acc::zero()
    }

    fn on_int(&mut self, _env: &Env, _i: i64) -> Self::Acc {
        Self::Acc::zero()
    }

    fn on_bool(&mut self, _env: &Env, _b: bool) -> Self::Acc {
        Self::Acc::zero()
    }

    fn on_symbol(&mut self, _env: &Env, _sym: &Symbol<L>) -> Self::Acc {
        Self::Acc::zero()
    }

    fn on_aloc(&mut self, _env: &Env, _loc: &ALoc) -> Self::Acc {
        Self::Acc::zero()
    }

    fn on_list<T, F>(&mut self, f: F, env: &Env, list: &[T]) -> Self::Acc
    where
        F: Fn(&mut Self, &Env, &T) -> Self::Acc,
    {
        self.list_fold_left(f, env, Self::Acc::zero(), list)
    }

    fn on_option<T, F>(&mut self, f: F, env: &Env, opt: &Option<T>) -> Self::Acc
    where
        F: Fn(&mut Self, &Env, &T) -> Self::Acc,
    {
        match opt {
            Some(x) => f(self, env, x),
            None => Self::Acc::zero(),
        }
    }

    fn list_fold_left<T, F>(&mut self, f: F, env: &Env, mut acc: Self::Acc, list: &[T]) -> Self::Acc
    where
        F: Fn(&mut Self, &Env, &T) -> Self::Acc,
    {
        for item in list {
            let item_acc = f(self, env, item);
            acc = Self::Acc::plus(acc, item_acc);
        }
        acc
    }
}

pub trait TyMapReduceBase<Env, L> {
    type Acc: Monoid;

    fn on_string(&mut self, _env: &Env, s: String) -> (String, Self::Acc) {
        (s, Self::Acc::zero())
    }

    fn on_name(&mut self, _env: &Env, name: Name) -> (Name, Self::Acc) {
        (name, Self::Acc::zero())
    }

    fn on_bool(&mut self, _env: &Env, b: bool) -> (bool, Self::Acc) {
        (b, Self::Acc::zero())
    }

    fn on_int(&mut self, _env: &Env, i: i64) -> (i64, Self::Acc) {
        (i, Self::Acc::zero())
    }

    fn on_symbol(&mut self, _env: &Env, sym: Symbol<L>) -> (Symbol<L>, Self::Acc) {
        (sym, Self::Acc::zero())
    }

    fn on_aloc(&mut self, _env: &Env, loc: ALoc) -> (ALoc, Self::Acc) {
        (loc, Self::Acc::zero())
    }

    fn on_list<T: Clone, U, F>(&mut self, f: F, env: &Env, list: &[T]) -> (Vec<U>, Self::Acc)
    where
        F: Fn(&mut Self, &Env, T) -> (U, Self::Acc),
    {
        self.list_fold_left(f, env, (Vec::new(), Self::Acc::zero()), list)
    }

    fn on_option<T, U, F>(&mut self, f: F, env: &Env, opt: Option<T>) -> (Option<U>, Self::Acc)
    where
        F: Fn(&mut Self, &Env, T) -> (U, Self::Acc),
    {
        match opt {
            Some(x) => {
                let (y, acc) = f(self, env, x);
                (Some(y), acc)
            }
            None => (None, Self::Acc::zero()),
        }
    }

    fn list_fold_left<T: Clone, U, F>(
        &mut self,
        f: F,
        env: &Env,
        acc: (Vec<U>, Self::Acc),
        list: &[T],
    ) -> (Vec<U>, Self::Acc)
    where
        F: Fn(&mut Self, &Env, T) -> (U, Self::Acc),
    {
        let (mut result, mut total_acc) = acc;
        for item in list {
            let (mapped, item_acc) = f(self, env, item.clone());
            result.push(mapped);
            total_acc = Self::Acc::plus(total_acc, item_acc);
        }
        (result, total_acc)
    }
}

impl Monoid for () {
    fn zero() -> Self {}

    fn plus(_a: Self, _b: Self) -> Self {}
}

impl Monoid for i64 {
    fn zero() -> Self {
        0
    }

    fn plus(a: Self, b: Self) -> Self {
        a + b
    }
}

impl Monoid for usize {
    fn zero() -> Self {
        0
    }

    fn plus(a: Self, b: Self) -> Self {
        a + b
    }
}

impl Monoid for bool {
    fn zero() -> Self {
        false
    }

    fn plus(a: Self, b: Self) -> Self {
        a || b
    }
}

impl<T> Monoid for Vec<T> {
    fn zero() -> Self {
        Vec::new()
    }

    fn plus(mut a: Self, b: Self) -> Self {
        a.extend(b);
        a
    }
}

impl<T> Monoid for Option<T> {
    fn zero() -> Self {
        None
    }

    fn plus(a: Self, b: Self) -> Self {
        a.or(b)
    }
}
