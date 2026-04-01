/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Port of `services/code_action/__tests__/insert_type_utils_tests.ml`

use std::sync::Arc;

use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_common_ty::ty::AnyKind;
use flow_common_ty::ty::BotKind;
use flow_common_ty::ty::Ty;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;

use crate::insert_type;
use crate::insert_type_utils::StylizeTyMapper;

fn stylize(t: Arc<Ty<ALoc>>) -> Arc<Ty<ALoc>> {
    use flow_common_ty::ty::TyEndoTy;
    let mut mapper = StylizeTyMapper::new();
    mapper.on_t(&Loc::default(), t)
}

//   >::: [

// ( "Stylize union: number with number literal" >:: fun ctxt ->
// );
#[test]
fn stylize_union_number_with_number_literal() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Num),
        Arc::new(Ty::NumLit("1".into())),
        Arc::from([]),
    ));
    let t_exp = Arc::new(Ty::Num);
    assert_eq!(t_exp, stylize(t_in));
}

// ( "Stylize union: string with string literal" >:: fun ctxt ->
// );
#[test]
fn stylize_union_string_with_string_literal() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::StrLit(Name::new("foo"))),
        Arc::new(Ty::Str),
        Arc::from([]),
    ));
    let t_exp = Arc::new(Ty::Str);
    assert_eq!(t_exp, stylize(t_in));
}

// ( "Stylize union: true and false" >:: fun ctxt ->
// );
#[test]
fn stylize_union_true_and_false() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::BoolLit(true)),
        Arc::new(Ty::BoolLit(false)),
        Arc::from([]),
    ));
    let t_exp = Arc::new(Ty::Bool);
    assert_eq!(t_exp, stylize(t_in));
}

// ( "Stylize union: true and bool" >:: fun ctxt ->
// );
#[test]
fn stylize_union_true_and_bool() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::BoolLit(true)),
        Arc::new(Ty::Bool),
        Arc::from([]),
    ));
    let t_exp = Arc::new(Ty::Bool);
    assert_eq!(t_exp, stylize(t_in));
}

// ( "Stylize union: string and number literals" >:: fun ctxt ->
// );
#[test]
fn stylize_union_string_number_literals() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Str),
        Arc::new(Ty::NumLit("1".into())),
        Arc::from([Arc::new(Ty::NumLit("2".into()))]),
    ));
    let t_exp = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::NumLit("1".into())),
        Arc::new(Ty::NumLit("2".into())),
        Arc::from([Arc::new(Ty::Str)]),
    ));
    assert_eq!(t_exp, stylize(t_in));
}

// ( "Sort types: numeric literals" >:: fun ctxt ->
// );
#[test]
fn sort_types_numeric_literals() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::NumLit("5".into())),
        Arc::new(Ty::NumLit("11".into())),
        Arc::from([
            Arc::new(Ty::NumLit("1".into())),
            Arc::new(Ty::NumLit("2".into())),
        ]),
    ));
    let t_exp = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::NumLit("1".into())),
        Arc::new(Ty::NumLit("2".into())),
        Arc::from([
            Arc::new(Ty::NumLit("5".into())),
            Arc::new(Ty::NumLit("11".into())),
        ]),
    ));
    assert_eq!(t_exp, insert_type::simplify(t_in));
}

// ( "Sort types: top, any" >:: fun ctxt ->
// );
#[test]
fn sort_types_top_any() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Top),
        Arc::new(Ty::Any(AnyKind::Annotated(ALoc::none()))),
        Arc::from([]),
    ));
    let t_exp = Arc::new(Ty::Top);
    assert_eq!(t_exp, insert_type::simplify(t_in));
}

// ( "Sort types: bot, any" >:: fun ctxt ->
// );
#[test]
fn sort_types_bot_any() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Bot(BotKind::EmptyType)),
        Arc::new(Ty::Any(AnyKind::Annotated(ALoc::none()))),
        Arc::from([]),
    ));
    let t_exp = Arc::new(Ty::Any(AnyKind::Annotated(ALoc::none())));
    assert_eq!(t_exp, insert_type::simplify(t_in));
}

// ( "Sort types: any first" >:: fun ctxt ->
//     Union (false, Void, Any (Annotated ALoc.none), [Null; Str; NumLit "5"; Bool])
//     Union (false, Any (Annotated ALoc.none), Void, [Null; Bool; NumLit "5"; Str])
// );
#[test]
fn sort_types_any_first() {
    let t_in = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Void),
        Arc::new(Ty::Any(AnyKind::Annotated(ALoc::none()))),
        Arc::from([
            Arc::new(Ty::Null),
            Arc::new(Ty::Str),
            Arc::new(Ty::NumLit("5".into())),
            Arc::new(Ty::Bool),
        ]),
    ));
    let t_exp = Arc::new(Ty::Union(
        false,
        Arc::new(Ty::Any(AnyKind::Annotated(ALoc::none()))),
        Arc::new(Ty::Void),
        Arc::from([
            Arc::new(Ty::Null),
            Arc::new(Ty::Bool),
            Arc::new(Ty::NumLit("5".into())),
            Arc::new(Ty::Str),
        ]),
    ));
    assert_eq!(t_exp, insert_type::simplify(t_in));
}
