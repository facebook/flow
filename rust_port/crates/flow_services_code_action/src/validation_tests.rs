/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_aloc::ALoc;
use flow_common_ty::ty::AnyKind;
use flow_common_ty::ty::Ty;
use flow_common_ty::ty::UnsoundnessKind;
use flow_parser::loc_sig::LocSig;

use crate::insert_type_utils::validator;
use crate::insert_type_utils::validator::ValidationError;

// Valid types
#[test]
fn any_annotated() {
    let t = Arc::new(Ty::Any(AnyKind::Annotated(ALoc::none())));
    let (_t, errs) = validator::validate_type(1000, &|aloc: &ALoc| aloc.to_loc_exn().clone(), t);
    assert_eq!(errs.len(), 0);
}

#[test]
fn any_bound_function_this() {
    let t = Arc::new(Ty::Any(AnyKind::Unsound(
        UnsoundnessKind::BoundFunctionThis,
    )));
    let (_t, errs) = validator::validate_type(1000, &|aloc: &ALoc| aloc.to_loc_exn().clone(), t);
    assert_eq!(errs.len(), 0);
}

// Invalid type (number | any(unsound)) - raises exception
#[test]
fn any_unsound_unresolved_type() {
    let t = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Num),
        Arc::new(Ty::Any(AnyKind::Unsound(UnsoundnessKind::UnresolvedType))),
        Arc::from([]),
    ));
    let (_t, errs) = validator::validate_type(1000, &|aloc: &ALoc| aloc.to_loc_exn().clone(), t);
    assert_eq!(
        errs,
        vec![ValidationError::AnyUnsound(UnsoundnessKind::UnresolvedType)]
    );
}

// Type too big - raises exception
#[test]
fn type_too_big() {
    let t = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Num),
        Arc::new(Ty::Num),
        Arc::from([]),
    ));
    let (_t, errs) = validator::validate_type(2, &|aloc: &ALoc| aloc.to_loc_exn().clone(), t);
    assert_eq!(
        errs,
        vec![ValidationError::TooBig {
            size_limit: 2,
            size: Some(3),
        }]
    );
}
