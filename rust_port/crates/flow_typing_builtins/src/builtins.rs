/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::rc::Rc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_aloc::ALoc;
use flow_common::flow_import_specifier::Userland;
use flow_common::reason::Reason;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::Type;

pub type LazyVal<'a, CX> =
    Rc<flow_lazy::Lazy<CX, (ALoc, Type), Box<dyn FnOnce(&CX) -> (ALoc, Type) + 'a>>>;

pub type LazyModuleType<'a, CX> =
    Rc<flow_lazy::Lazy<CX, ModuleType, Box<dyn FnOnce(&CX) -> ModuleType + 'a>>>;

pub type LazyModule<'a, CX> = Rc<
    flow_lazy::Lazy<
        CX,
        (Reason, LazyModuleType<'a, CX>),
        Box<dyn FnOnce(&CX) -> (Reason, LazyModuleType<'a, CX>) + 'a>,
    >,
>;

pub struct Builtins<'a, CX> {
    original_global_values: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
    original_global_types: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
    original_global_modules: FlowOrdMap<FlowSmolStr, LazyModule<'a, CX>>,
    source_cx: Option<CX>,
    type_mapper: Rc<dyn Fn(&CX, &CX, Type) -> Type + 'a>,
    module_type_mapper: Rc<dyn Fn(&CX, &CX, &ModuleType) -> ModuleType + 'a>,
    mapped_global_names: RefCell<HashMap<FlowSmolStr, (ALoc, Type)>>,
    mapped_global_types: RefCell<HashMap<FlowSmolStr, (ALoc, Type)>>,
    mapped_global_modules: RefCell<HashMap<FlowSmolStr, (Reason, LazyModuleType<'a, CX>)>>,
}

impl<CX> std::fmt::Debug for Builtins<'_, CX> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Builtins")
            .field(
                "original_global_values",
                &format!("<{} entries>", self.original_global_values.len()),
            )
            .field(
                "original_global_types",
                &format!("<{} entries>", self.original_global_types.len()),
            )
            .field(
                "original_global_modules",
                &format!("<{} entries>", self.original_global_modules.len()),
            )
            .finish()
    }
}

impl<'a, CX: Clone + 'a> Builtins<'a, CX> {
    pub fn builtin_ordinary_name_set(&self) -> BTreeSet<FlowSmolStr> {
        let mut result = BTreeSet::new();
        for k in self.original_global_values.keys() {
            result.insert(k.dupe());
        }
        for k in self.original_global_types.keys() {
            result.insert(k.dupe());
        }
        result
    }

    pub fn builtin_modules_set(&self) -> BTreeSet<FlowSmolStr> {
        self.original_global_modules.keys().duped().collect()
    }

    pub fn get_builtin_value_opt(&self, cx: &CX, name: &str) -> Option<(ALoc, Type)> {
        if let Some(v) = self.mapped_global_names.borrow().get(name) {
            return Some((v.0.dupe(), v.1.dupe()));
        }
        match self.original_global_values.get(name) {
            None => None,
            Some(lazy_val) => {
                let source_cx = self.source_cx.as_ref().unwrap_or(cx);
                let (l, v) = lazy_val.get_forced(source_cx).clone();
                let v = (self.type_mapper)(source_cx, cx, v);
                self.mapped_global_names
                    .borrow_mut()
                    .insert(FlowSmolStr::from(name), (l.dupe(), v.dupe()));
                Some((l, v))
            }
        }
    }

    fn get_builtin_type_opt_inner(&self, cx: &CX, name: &str) -> Option<(ALoc, Type)> {
        if let Some(v) = self.mapped_global_types.borrow().get(name) {
            return Some((v.0.dupe(), v.1.dupe()));
        }
        match self.original_global_types.get(name) {
            None => None,
            Some(lazy_val) => {
                let source_cx = self.source_cx.as_ref().unwrap_or(cx);
                let (l, v) = lazy_val.get_forced(source_cx).clone();
                let v = (self.type_mapper)(source_cx, cx, v);
                self.mapped_global_types
                    .borrow_mut()
                    .insert(FlowSmolStr::from(name), (l.dupe(), v.dupe()));
                Some((l, v))
            }
        }
    }

    pub fn get_builtin_type_opt(&self, cx: &CX, name: &str) -> Option<(ALoc, Type)> {
        match self.get_builtin_type_opt_inner(cx, name) {
            None => self.get_builtin_value_opt(cx, name),
            v_opt => v_opt,
        }
    }

    pub fn get_builtin_module_opt(
        &self,
        cx: &CX,
        name: &Userland,
    ) -> Option<(Reason, LazyModuleType<'a, CX>)> {
        let name_str = name.as_str();
        if let Some(v) = self.mapped_global_modules.borrow().get(name_str) {
            return Some((v.0.dupe(), v.1.dupe()));
        }
        match self.original_global_modules.get(name_str) {
            None => None,
            Some(lazy_val) => {
                let source_cx = self.source_cx.as_ref().unwrap_or(cx);
                let (r, lazy_module) = lazy_val.get_forced(source_cx);
                let module = lazy_module.get_forced(source_cx).dupe();
                let mapped_module: LazyModuleType<'a, CX> = Rc::new(flow_lazy::Lazy::new_forced(
                    (self.module_type_mapper)(source_cx, cx, &module),
                ));
                self.mapped_global_modules.borrow_mut().insert(
                    FlowSmolStr::from(name_str),
                    (r.dupe(), mapped_module.dupe()),
                );
                Some((r.dupe(), mapped_module))
            }
        }
    }

    pub fn of_name_map(
        type_mapper: Rc<dyn Fn(&CX, &CX, Type) -> Type + 'a>,
        module_type_mapper: Rc<dyn Fn(&CX, &CX, &ModuleType) -> ModuleType + 'a>,
        values: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
        types: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
        modules: FlowOrdMap<FlowSmolStr, LazyModule<'a, CX>>,
    ) -> Self {
        Self::of_name_map_with_source_cx_opt(
            None,
            type_mapper,
            module_type_mapper,
            values,
            types,
            modules,
        )
    }

    pub fn of_name_map_with_source_cx(
        source_cx: CX,
        type_mapper: Rc<dyn Fn(&CX, &CX, Type) -> Type + 'a>,
        module_type_mapper: Rc<dyn Fn(&CX, &CX, &ModuleType) -> ModuleType + 'a>,
        values: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
        types: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
        modules: FlowOrdMap<FlowSmolStr, LazyModule<'a, CX>>,
    ) -> Self {
        Self::of_name_map_with_source_cx_opt(
            Some(source_cx),
            type_mapper,
            module_type_mapper,
            values,
            types,
            modules,
        )
    }

    fn of_name_map_with_source_cx_opt(
        source_cx: Option<CX>,
        type_mapper: Rc<dyn Fn(&CX, &CX, Type) -> Type + 'a>,
        module_type_mapper: Rc<dyn Fn(&CX, &CX, &ModuleType) -> ModuleType + 'a>,
        values: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
        types: FlowOrdMap<FlowSmolStr, LazyVal<'a, CX>>,
        modules: FlowOrdMap<FlowSmolStr, LazyModule<'a, CX>>,
    ) -> Self {
        Builtins {
            original_global_values: values,
            original_global_types: types,
            original_global_modules: modules,
            source_cx,
            type_mapper,
            module_type_mapper,
            mapped_global_names: RefCell::new(HashMap::new()),
            mapped_global_types: RefCell::new(HashMap::new()),
            mapped_global_modules: RefCell::new(HashMap::new()),
        }
    }

    pub fn empty() -> Self {
        Builtins {
            original_global_values: FlowOrdMap::new(),
            original_global_types: FlowOrdMap::new(),
            original_global_modules: FlowOrdMap::new(),
            source_cx: None,
            type_mapper: Rc::new(|_src_cx: &CX, _dst_cx: &CX, t| t),
            module_type_mapper: Rc::new(|_src_cx: &CX, _dst_cx: &CX, m: &ModuleType| m.dupe()),
            mapped_global_names: RefCell::new(HashMap::new()),
            mapped_global_types: RefCell::new(HashMap::new()),
            mapped_global_modules: RefCell::new(HashMap::new()),
        }
    }
}

impl<'a, CX: Clone + 'a> Default for Builtins<'a, CX> {
    fn default() -> Self {
        Self::empty()
    }
}
