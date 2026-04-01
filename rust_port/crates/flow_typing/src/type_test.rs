/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;

use dupe::Dupe;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::locationless_reason;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::constraint::forcing_state::ForcingState;

fn assert_forced_to_any(s: &ForcingState) {
    match &*s.force(&(), |r| any_t::error(r.dupe())) {
        TypeInner::AnyT(_, _) => {}
        _ => panic!("Invalid type"),
    }
}

#[test]
fn invalid_self_recursive() {
    let s: Rc<RefCell<Option<ForcingState>>> = Rc::new(RefCell::new(None));
    let s_clone = s.clone();
    let state = ForcingState::of_lazy_t(
        locationless_reason(VirtualReasonDesc::RNull),
        move |_: &()| {
            let s_ref = s_clone.borrow();
            let inner_state = s_ref.as_ref().unwrap();
            inner_state.force(&(), |r| any_t::error(r.dupe()))
        },
    );
    *s.borrow_mut() = Some(state);
    let s_ref = s.borrow();
    let state = s_ref.as_ref().unwrap();
    assert_forced_to_any(state);
}

#[test]
fn invalid_mutually_recursive() {
    let s1: Rc<RefCell<Option<ForcingState>>> = Rc::new(RefCell::new(None));
    let s2: Rc<RefCell<Option<ForcingState>>> = Rc::new(RefCell::new(None));

    let s2_for_s1 = s2.clone();
    let state1 = ForcingState::of_lazy_t(
        locationless_reason(VirtualReasonDesc::RNull),
        move |_: &()| {
            let s2_ref = s2_for_s1.borrow();
            let inner_state = s2_ref.as_ref().unwrap();
            inner_state.force(&(), |r| any_t::error(r.dupe()))
        },
    );

    let s1_for_s2 = s1.clone();
    let state2 = ForcingState::of_lazy_t(
        locationless_reason(VirtualReasonDesc::RNull),
        move |_: &()| {
            let s1_ref = s1_for_s2.borrow();
            let inner_state = s1_ref.as_ref().unwrap();
            inner_state.force(&(), |r| any_t::error(r.dupe()))
        },
    );

    *s1.borrow_mut() = Some(state1);
    *s2.borrow_mut() = Some(state2);

    let s1_ref = s1.borrow();
    let s2_ref = s2.borrow();
    assert_forced_to_any(s1_ref.as_ref().unwrap());
    assert_forced_to_any(s2_ref.as_ref().unwrap());
}

#[test]
fn invalid_self_recursive_mapped() {
    let s: Rc<RefCell<Option<ForcingState>>> = Rc::new(RefCell::new(None));
    let s_clone = s.clone();
    let state = ForcingState::of_lazy_t(
        locationless_reason(VirtualReasonDesc::RNull),
        move |_: &()| {
            let s_ref = s_clone.borrow();
            let inner_state = s_ref.as_ref().unwrap();
            inner_state.force(&(), |r| any_t::error(r.dupe()))
        },
    );
    *s.borrow_mut() = Some(state);

    let s_ref = s.borrow();
    let state = s_ref.as_ref().unwrap();
    let state_copy = state.copy(&(), |_cx: &(), r| any_t::error(r.dupe()), |_, _, _| {});
    assert_forced_to_any(&state_copy);
}

#[test]
fn invalid_self_recursive_force_twice() {
    let s: Rc<RefCell<Option<ForcingState>>> = Rc::new(RefCell::new(None));
    let s_clone = s.clone();
    let state = ForcingState::of_lazy_t(
        locationless_reason(VirtualReasonDesc::RNull),
        move |_: &()| {
            let s_ref = s_clone.borrow();
            let inner_state = s_ref.as_ref().unwrap();
            inner_state.force(&(), |r| any_t::error(r.dupe()));
            inner_state.force(&(), |r| any_t::error(r.dupe()))
        },
    );
    *s.borrow_mut() = Some(state);

    let s_ref = s.borrow();
    let state = s_ref.as_ref().unwrap();
    let state_copy = state.copy(&(), |_cx: &(), r| any_t::error(r.dupe()), |_, _, _| {});
    assert_forced_to_any(&state_copy);
}
