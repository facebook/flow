/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;

use flow_aloc::ALoc;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;

fn id_nop(_cx: &Context, _name: &str, _loc: ALoc) -> bool {
    false
}

fn literal_nop(_cx: &Context, _loc: ALoc) -> bool {
    false
}

fn jsx_nop(_cx: &Context, _name: &str, _loc: ALoc) -> bool {
    false
}

fn obj_prop_decl_nop(_cx: &Context, _name: &str, _loc: ALoc) -> bool {
    false
}

fn obj_to_obj_nop(_cx: &Context, _t1: &Type, _t2: &Type) {}

// This type represents the possible definition-points for an lvalue.
pub enum Def {
    /// Given a variable declaration such as:
    ///
    /// ```js
    /// var a = 42; // <-- this
    /// var b; // <-- this
    /// ```
    ///
    /// We emit the type of the variable, given as an annotation or inferred by
    /// looking at its assignments, as the "definition" for the lvalue.
    Val(Type),
    /// Given a destructuring pattern for an initialization such as:
    ///
    /// ```js
    /// var {
    ///   a // <-- this
    /// } = {a: {b: 42}};
    /// ```
    ///
    /// We emit the tvar for the "parent pattern" being destructured so that we
    /// may extract the type of the corresponding property -- and thus the location
    /// of that type -- as the "definition" for the binding generated from a
    /// destructuring pattern.
    Parent(Type),
    /// For assignments, we consider lvalues to have the same "definition" as
    /// corresponding rvalues: both kinds of references point to the declaration site.
    Id,
}

pub struct HookState {
    pub id_hook: fn(&Context, &str, ALoc) -> bool,
    pub literal_hook: fn(&Context, ALoc) -> bool,
    pub jsx_hook: fn(&Context, &str, ALoc) -> bool,
    pub obj_prop_decl_hook: fn(&Context, &str, ALoc) -> bool,
    // Called when ObjT 1 ~> ObjT 2
    pub obj_to_obj_hook: fn(&Context, &Type, &Type),
    // Flag to indicate if the current check is for IDE services
    pub for_ide: bool,
}

pub const NOP_HOOK_STATE: HookState = HookState {
    id_hook: id_nop,
    literal_hook: literal_nop,
    jsx_hook: jsx_nop,
    obj_prop_decl_hook: obj_prop_decl_nop,
    obj_to_obj_hook: obj_to_obj_nop,
    for_ide: false,
};

thread_local! {
    static HOOK_STATE: RefCell<HookState> = RefCell::new(HookState {
        id_hook: id_nop,
        literal_hook: literal_nop,
        jsx_hook: jsx_nop,
        obj_prop_decl_hook: obj_prop_decl_nop,
        obj_to_obj_hook: obj_to_obj_nop,
        for_ide: false,
    });
}

pub fn set_id_hook(hook: fn(&Context, &str, ALoc) -> bool) {
    HOOK_STATE.with(|state| {
        state.borrow_mut().id_hook = hook;
    });
}

pub fn set_literal_hook(hook: fn(&Context, ALoc) -> bool) {
    HOOK_STATE.with(|state| {
        state.borrow_mut().literal_hook = hook;
    });
}

pub fn set_jsx_hook(hook: fn(&Context, &str, ALoc) -> bool) {
    HOOK_STATE.with(|state| {
        state.borrow_mut().jsx_hook = hook;
    });
}

pub fn set_obj_prop_decl_hook(hook: fn(&Context, &str, ALoc) -> bool) {
    HOOK_STATE.with(|state| {
        state.borrow_mut().obj_prop_decl_hook = hook;
    });
}

pub fn set_obj_to_obj_hook(hook: fn(&Context, &Type, &Type)) {
    HOOK_STATE.with(|state| {
        state.borrow_mut().obj_to_obj_hook = hook;
    });
}

pub fn set_for_ide(flag: bool) {
    HOOK_STATE.with(|state| {
        state.borrow_mut().for_ide = flag;
    });
}

pub fn with_for_ide<T>(enabled: bool, f: impl FnOnce() -> T) -> T {
    let old_flag = HOOK_STATE.with(|state| state.borrow().for_ide);
    set_for_ide(enabled);
    let result = f();
    set_for_ide(old_flag);
    result
}

pub fn reset_hooks() {
    HOOK_STATE.with(|state| {
        let mut s = state.borrow_mut();
        s.id_hook = id_nop;
        s.literal_hook = literal_nop;
        s.jsx_hook = jsx_nop;
        s.obj_prop_decl_hook = obj_prop_decl_nop;
        s.obj_to_obj_hook = obj_to_obj_nop;
        s.for_ide = false;
    });
}

pub fn dispatch_id_hook<'cx>(cx: &Context<'cx>, name: &str, loc: ALoc) -> bool {
    let hook = HOOK_STATE.with(|state| state.borrow().id_hook);
    hook(cx, name, loc)
}

pub fn dispatch_literal_hook<'cx>(cx: &Context<'cx>, loc: ALoc) -> bool {
    let hook = HOOK_STATE.with(|state| state.borrow().literal_hook);
    hook(cx, loc)
}

pub fn dispatch_jsx_hook<'cx>(cx: &Context<'cx>, name: &str, loc: ALoc) -> bool {
    let hook = HOOK_STATE.with(|state| state.borrow().jsx_hook);
    hook(cx, name, loc)
}

pub fn dispatch_obj_prop_decl_hook<'cx>(cx: &Context<'cx>, name: &str, loc: ALoc) -> bool {
    let hook = HOOK_STATE.with(|state| state.borrow().obj_prop_decl_hook);
    hook(cx, name, loc)
}

pub fn dispatch_obj_to_obj_hook<'cx>(cx: &Context<'cx>, t1: &Type, t2: &Type) {
    let hook = HOOK_STATE.with(|state| state.borrow().obj_to_obj_hook);
    hook(cx, t1, t2);
}

pub fn is_for_ide() -> bool {
    // !hook_state.for_ide
    HOOK_STATE.with(|state| state.borrow().for_ide)
}
