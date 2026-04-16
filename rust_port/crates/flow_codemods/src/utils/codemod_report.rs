/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

pub struct ReporterOptions {
    pub strip_root: Option<PathBuf>,
    pub exact_by_default: bool,
}

pub enum Reporter<A> {
    StringReporter(Box<dyn Fn(&ReporterOptions, &A) -> String + Send + Sync>),
    UnitReporter(Box<dyn Fn(&ReporterOptions, &A) + Send + Sync>),
}

pub struct CodemodReport<A> {
    pub report: Reporter<A>,
    pub combine: Box<dyn Fn(A, A) -> A + Send + Sync>,
    pub empty: A,
}

pub fn unit_reporter() -> CodemodReport<()> {
    let report = Reporter::UnitReporter(Box::new(|_, _| ()));
    let combine = Box::new(|_, _| ());
    let empty = ();
    CodemodReport {
        report,
        combine,
        empty,
    }
}

pub trait CodemodReportS {
    fn report(opts: &ReporterOptions, value: &Self) -> String;
    fn combine(a: Self, b: Self) -> Self;
    fn empty() -> Self;
}
