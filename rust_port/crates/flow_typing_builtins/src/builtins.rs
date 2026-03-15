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
use once_cell::unsync::Lazy;

pub struct Builtins {
    original_global_values:
        FlowOrdMap<FlowSmolStr, Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>>,
    original_global_types:
        FlowOrdMap<FlowSmolStr, Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>>,
    original_global_modules: FlowOrdMap<
        FlowSmolStr,
        Rc<
            Lazy<
                (
                    Reason,
                    Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>>,
                ),
                Box<
                    dyn FnOnce() -> (
                        Reason,
                        Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>>,
                    ),
                >,
            >,
        >,
    >,
    type_mapper: Rc<dyn Fn(Type) -> Type>,
    module_type_mapper: Rc<dyn Fn(&ModuleType) -> ModuleType>,
    mapped_global_names: RefCell<HashMap<FlowSmolStr, (ALoc, Type)>>,
    mapped_global_types: RefCell<HashMap<FlowSmolStr, (ALoc, Type)>>,
    mapped_global_modules: RefCell<HashMap<FlowSmolStr, (Reason, ModuleType)>>,
}

impl std::fmt::Debug for Builtins {
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

impl Builtins {
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

    pub fn get_builtin_value_opt(&self, name: &str) -> Option<(ALoc, Type)> {
        if let Some(v) = self.mapped_global_names.borrow().get(name) {
            return Some((v.0.dupe(), v.1.dupe()));
        }
        match self.original_global_values.get(name) {
            None => None,
            Some(lazy_val) => {
                let (l, v) = (**lazy_val).clone();
                let v = (self.type_mapper)(v);
                self.mapped_global_names
                    .borrow_mut()
                    .insert(FlowSmolStr::from(name), (l.dupe(), v.dupe()));
                Some((l, v))
            }
        }
    }

    fn get_builtin_type_opt_inner(&self, name: &str) -> Option<(ALoc, Type)> {
        if let Some(v) = self.mapped_global_types.borrow().get(name) {
            return Some((v.0.dupe(), v.1.dupe()));
        }
        match self.original_global_types.get(name) {
            None => None,
            Some(lazy_val) => {
                let (l, v) = (**lazy_val).clone();
                let v = (self.type_mapper)(v);
                self.mapped_global_types
                    .borrow_mut()
                    .insert(FlowSmolStr::from(name), (l.dupe(), v.dupe()));
                Some((l, v))
            }
        }
    }

    pub fn get_builtin_type_opt(&self, name: &str) -> Option<(ALoc, Type)> {
        match self.get_builtin_type_opt_inner(name) {
            None => self.get_builtin_value_opt(name),
            v_opt => v_opt,
        }
    }

    pub fn get_builtin_module_opt(&self, name: &Userland) -> Option<(Reason, ModuleType)> {
        let name_str = name.as_str();
        if let Some(v) = self.mapped_global_modules.borrow().get(name_str) {
            return Some((v.0.dupe(), v.1.dupe()));
        }
        match self.original_global_modules.get(name_str) {
            None => None,
            Some(lazy_val) => {
                let (r, lazy_module) = &***lazy_val;
                let mapped_module = (self.module_type_mapper)(lazy_module);
                self.mapped_global_modules.borrow_mut().insert(
                    FlowSmolStr::from(name_str),
                    (r.dupe(), mapped_module.dupe()),
                );
                Some((r.dupe(), mapped_module))
            }
        }
    }

    pub fn of_name_map(
        type_mapper: Rc<dyn Fn(Type) -> Type>,
        module_type_mapper: Rc<dyn Fn(&ModuleType) -> ModuleType>,
        values: FlowOrdMap<FlowSmolStr, Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>>,
        types: FlowOrdMap<FlowSmolStr, Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>>,
        modules: FlowOrdMap<
            FlowSmolStr,
            Rc<
                Lazy<
                    (
                        Reason,
                        Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>>,
                    ),
                    Box<
                        dyn FnOnce() -> (
                            Reason,
                            Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>>,
                        ),
                    >,
                >,
            >,
        >,
    ) -> Self {
        Builtins {
            original_global_values: values,
            original_global_types: types,
            original_global_modules: modules,
            type_mapper,
            module_type_mapper,
            mapped_global_names: RefCell::new(HashMap::new()),
            mapped_global_types: RefCell::new(HashMap::new()),
            mapped_global_modules: RefCell::new(HashMap::new()),
        }
    }

    pub fn empty() -> Self {
        Self::of_name_map(
            Rc::new(|t| t),
            Rc::new(|m| m.dupe()),
            FlowOrdMap::new(),
            FlowOrdMap::new(),
            FlowOrdMap::new(),
        )
    }
}

impl Default for Builtins {
    fn default() -> Self {
        Self::empty()
    }
}
