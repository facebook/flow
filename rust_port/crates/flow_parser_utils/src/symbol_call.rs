/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The calls that mint a symbol: `Symbol(...)` and `Symbol.for(...)`.
//!
//! Read as a value, either one returns a symbol no other expression can
//! produce, so where the result is bound once and for good it can be given an
//! identity and used as a property key. Both the checking and the signature
//! pipeline decide that from the same shape here, and each answers on its own
//! whether the `Symbol` this reads is the global one.

use std::ops::Deref;

use dupe::Dupe;
use flow_parser::ast::Identifier;
use flow_parser::ast::expression;

/// The `Symbol` of a call to `Symbol(...)` or to the registry lookup
/// `Symbol.for(...)`, or `None` for any other expression. The identifier is
/// returned rather than a bare yes, since the caller still has to resolve it:
/// a local binding of that name shadows the global constructor, and then the
/// call mints nothing.
///
/// What a caller can settle is whether the name is bound in the file, not which
/// declaration a global name came from, so a library definition that writes its
/// own `Symbol` mints as the builtin does. TypeScript resolves the callee to the
/// one declaration it ships and does not.
pub fn symbol_constructor_call<'a, M: Dupe, T: Dupe>(
    expr: &'a expression::Expression<M, T>,
) -> Option<&'a Identifier<M, T>> {
    use expression::ExpressionInner as E;

    let E::Call { inner, .. } = expr.deref() else {
        return None;
    };
    let callee = match inner.callee.deref() {
        E::Member { inner: m, .. }
            if matches!(
                &m.property,
                expression::member::Property::PropertyIdentifier(id)
                    if id.name.as_str() == "for"
            ) =>
        {
            &m.object
        }
        _ => &inner.callee,
    };
    match callee.deref() {
        E::Identifier { inner: id, .. } if id.name.as_str() == "Symbol" => Some(id),
        _ => None,
    }
}
