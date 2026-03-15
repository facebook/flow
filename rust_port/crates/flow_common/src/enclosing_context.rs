/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_aloc::ALoc;

/// Information about the enclosing syntactic context of an expression. Each
/// one of these variants corresponds to the context of `C` in the expression
/// or statement shown in comment above it.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EnclosingContext {
    NoContext,
    // switch(C){...}
    SwitchTestContext {
        case_test_loc: ALoc,
        switch_discriminant_loc: ALoc,
    },
    // if(C){}
    // while(C){}
    // C?e1:e2
    // invariant(C)
    OtherTestContext,
    // o[C]
    // o[C]=e
    // {[C]:e}
    // o[C]()
    IndexContext,
    // <C />
    JsxTitleNameContext,
    // <Foo bar={C}>{C}</Foo>
    JsxAttrOrChildrenContext,
    // if (x ==/=== C) {} when C is literal expression
    LiteralTestContext,
    // match (x) { C => e }
    MatchPattern,
    StrictComparison,
}

impl EnclosingContext {
    pub fn is_conditional_test_context(&self) -> bool {
        matches!(
            self,
            EnclosingContext::SwitchTestContext { .. } | EnclosingContext::OtherTestContext
        )
    }
}
