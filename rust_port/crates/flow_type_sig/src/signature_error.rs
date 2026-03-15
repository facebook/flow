/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
pub use flow_parser::ast_utils::expression_sort::ExpressionSort;

use crate::expected_annotation_sort::ExpectedAnnotationSort;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SignatureError<Loc> {
    ExpectedAnnotation(Loc, ExpectedAnnotationSort),
    UnexpectedObjectKey(Loc, Loc),
    UnexpectedArraySpread(Loc, Loc),
    UnexpectedArrayHole(Loc),
    EmptyArray(Loc),
    EmptyObject(Loc),
    UnexpectedExpression(Loc, ExpressionSort),
}

impl<Loc> SignatureError<Loc> {
    pub fn iter(&self, mut f_loc: impl FnMut(&Loc)) {
        match self {
            Self::ExpectedAnnotation(loc, _) => {
                f_loc(loc);
            }
            Self::UnexpectedObjectKey(loc1, loc2) => {
                f_loc(loc1);
                f_loc(loc2);
            }
            Self::UnexpectedArraySpread(loc1, loc2) => {
                f_loc(loc1);
                f_loc(loc2);
            }
            Self::UnexpectedArrayHole(loc) => {
                f_loc(loc);
            }
            Self::EmptyArray(loc) => {
                f_loc(loc);
            }
            Self::EmptyObject(loc) => {
                f_loc(loc);
            }
            Self::UnexpectedExpression(loc, _) => {
                f_loc(loc);
            }
        }
    }

    pub fn map<CX, Loc2>(
        &self,
        cx: &mut CX,
        f: impl Fn(&mut CX, &Loc) -> Loc2,
    ) -> SignatureError<Loc2> {
        match self {
            Self::ExpectedAnnotation(loc, sort) => {
                SignatureError::ExpectedAnnotation(f(cx, loc), sort.dupe())
            }
            Self::UnexpectedObjectKey(loc1, loc2) => {
                SignatureError::UnexpectedObjectKey(f(cx, loc1), f(cx, loc2))
            }
            Self::UnexpectedArraySpread(loc1, loc2) => {
                SignatureError::UnexpectedArraySpread(f(cx, loc1), f(cx, loc2))
            }
            Self::UnexpectedArrayHole(loc) => SignatureError::UnexpectedArrayHole(f(cx, loc)),
            Self::EmptyArray(loc) => SignatureError::EmptyArray(f(cx, loc)),
            Self::EmptyObject(loc) => SignatureError::EmptyObject(f(cx, loc)),
            Self::UnexpectedExpression(loc, sort) => {
                SignatureError::UnexpectedExpression(f(cx, loc), *sort)
            }
        }
    }
}

impl<Loc: fmt::Display> fmt::Display for SignatureError<Loc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SignatureError::ExpectedAnnotation(loc, sort) => {
                write!(f, "ExpectedAnnotation ({}, {})", loc, sort)
            }
            SignatureError::UnexpectedObjectKey(loc1, loc2) => {
                write!(f, "UnexpectedObjectKey ({}, {})", loc1, loc2)
            }
            SignatureError::UnexpectedArraySpread(loc1, loc2) => {
                write!(f, "UnexpectedArraySpread ({}, {})", loc1, loc2)
            }
            SignatureError::UnexpectedArrayHole(loc) => {
                write!(f, "UnexpectedArrayHole ({})", loc)
            }
            SignatureError::EmptyArray(loc) => {
                write!(f, "EmptyArray ({})", loc)
            }
            SignatureError::EmptyObject(loc) => {
                write!(f, "EmptyObject ({})", loc)
            }
            SignatureError::UnexpectedExpression(loc, sort) => {
                write!(f, "UnexpectedExpression ({}, {:?})", loc, sort)
            }
        }
    }
}

impl<Loc: fmt::Display> fmt::Display for BindingValidation<Loc> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingValidation::ModuleOverride {
                name,
                override_binding_loc,
                existing_binding_loc,
            } => {
                write!(
                    f,
                    "ModuleOverride (name: {}, override_binding_loc: {}, existing_binding_loc: {})",
                    name, override_binding_loc, existing_binding_loc
                )
            }
            BindingValidation::NameOverride {
                name,
                override_binding_loc,
                existing_binding_loc,
            } => {
                write!(
                    f,
                    "NameOverride (name: {}, override_binding_loc: {}, existing_binding_loc: {})",
                    name, override_binding_loc, existing_binding_loc
                )
            }
            BindingValidation::NamespacedNameAlreadyBound {
                name,
                invalid_binding_loc,
                existing_binding_loc,
            } => {
                write!(
                    f,
                    "NamespacedNameAlreadyBound (name: {}, invalid_binding_loc: {}, existing_binding_loc: {})",
                    name, invalid_binding_loc, existing_binding_loc
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindingValidation<Loc> {
    ModuleOverride {
        name: FlowSmolStr,
        override_binding_loc: Loc,
        existing_binding_loc: Loc,
    },
    NameOverride {
        name: FlowSmolStr,
        override_binding_loc: Loc,
        existing_binding_loc: Loc,
    },
    NamespacedNameAlreadyBound {
        name: FlowSmolStr,
        invalid_binding_loc: Loc,
        existing_binding_loc: Loc,
    },
}

impl<Loc> BindingValidation<Loc> {
    pub fn iter(&self, mut f_loc: impl FnMut(&Loc)) {
        match self {
            Self::ModuleOverride {
                override_binding_loc,
                existing_binding_loc,
                ..
            } => {
                f_loc(override_binding_loc);
                f_loc(existing_binding_loc);
            }
            Self::NameOverride {
                override_binding_loc,
                existing_binding_loc,
                ..
            } => {
                f_loc(override_binding_loc);
                f_loc(existing_binding_loc);
            }
            Self::NamespacedNameAlreadyBound {
                invalid_binding_loc,
                existing_binding_loc,
                ..
            } => {
                f_loc(invalid_binding_loc);
                f_loc(existing_binding_loc);
            }
        }
    }

    pub fn map<CX, Loc2>(
        &self,
        cx: &mut CX,
        f: impl Fn(&mut CX, &Loc) -> Loc2,
    ) -> BindingValidation<Loc2> {
        match self {
            Self::ModuleOverride {
                name,
                override_binding_loc,
                existing_binding_loc,
            } => BindingValidation::ModuleOverride {
                name: name.dupe(),
                override_binding_loc: f(cx, override_binding_loc),
                existing_binding_loc: f(cx, existing_binding_loc),
            },
            Self::NameOverride {
                name,
                override_binding_loc,
                existing_binding_loc,
            } => BindingValidation::NameOverride {
                name: name.dupe(),
                override_binding_loc: f(cx, override_binding_loc),
                existing_binding_loc: f(cx, existing_binding_loc),
            },
            Self::NamespacedNameAlreadyBound {
                name,
                invalid_binding_loc,
                existing_binding_loc,
            } => BindingValidation::NamespacedNameAlreadyBound {
                name: name.dupe(),
                invalid_binding_loc: f(cx, invalid_binding_loc),
                existing_binding_loc: f(cx, existing_binding_loc),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum TolerableError<Loc> {
    SignatureVerificationError(SignatureError<Loc>),
    SignatureBindingValidationError(BindingValidation<Loc>),
}
