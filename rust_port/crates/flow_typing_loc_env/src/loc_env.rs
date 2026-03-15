/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! New Environment:
//! New environment maps locs to types using the ssa builder

use std::cell::RefCell;
use std::rc::Rc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::reason::Reason;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_env_builder::env_api::DefLocType;
use flow_env_builder::env_api::EnvInfo;
use flow_env_builder::env_api::EnvKey;
use flow_env_builder::env_api::EnvMap;
use flow_env_builder::name_def_types::EnvEntriesMap;
use flow_env_builder::name_def_types::HintMap;
use flow_env_builder::name_def_types::ScopeKind;
use flow_parser::loc_sig::LocSig;
use flow_typing_type::type_::ClassBinding;
use flow_typing_type::type_::LazyHintT;
use flow_typing_type::type_::PredFuncallInfo;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_util::mod_reason_of_t;
use once_cell::unsync::Lazy;

#[derive(Debug, Clone)]
pub struct TypeEntry {
    pub t: Type,
    pub state: Rc<RefCell<Lazy<Type, Box<dyn FnOnce() -> Type>>>>,
}

#[derive(Clone)]
pub struct LocEnv {
    pub types: EnvMap<ALoc, TypeEntry>,
    pub tparams: EnvMap<ALoc, (SubstName, TypeParam, Type)>,
    pub class_bindings: EnvMap<ALoc, ClassBinding>,
    pub class_stack: FlowVector<ALoc>,
    pub scope_kind: ScopeKind,
    pub ast_hint_map: HintMap,
    pub hint_map: EnvMap<ALoc, LazyHintT>,
    pub var_info: Rc<EnvInfo<ALoc>>,
    pub pred_func_map:
        FlowOrdMap<ALoc, Rc<Lazy<PredFuncallInfo, Box<dyn FnOnce() -> PredFuncallInfo>>>>,
    pub name_defs: EnvEntriesMap,
}

impl LocEnv {
    pub fn initialize(&mut self, def_loc_kind: DefLocType, loc: ALoc, state: TypeEntry) {
        let key = EnvKey::new(def_loc_kind, loc.dupe());
        if self.types.contains_key(&key) {
            panic!("{} already initialized", loc.debug_to_string(true));
        }
        self.types.insert(key, state);
    }

    pub fn update_reason(&mut self, def_loc_kind: DefLocType, loc: ALoc, reason: Reason) {
        let f = |_: Reason| reason.dupe();
        let key = EnvKey::new(def_loc_kind, loc);
        match self.types.get(&key) {
            Some(TypeEntry { t, state }) => {
                let new_entry = TypeEntry {
                    t: mod_reason_of_t(&f, t),
                    state: state.dupe(),
                };
                self.types.insert(key, new_entry);
            }
            None => panic!("Cannot update reason on non-existent entry"),
        }
    }

    pub fn find_write(&self, def_loc_kind: DefLocType, loc: ALoc) -> Option<&TypeEntry> {
        self.types.get(&EnvKey::new(def_loc_kind, loc))
    }

    pub fn find_ordinary_write(&self, loc: ALoc) -> Option<&TypeEntry> {
        self.find_write(DefLocType::OrdinaryNameLoc, loc)
    }

    pub fn empty(scope_kind: ScopeKind) -> Self {
        thread_local! {
            static CACHED: LocEnv = LocEnv {
                types: EnvMap::empty(),
                var_info: Rc::new(EnvInfo::empty()),
                tparams: EnvMap::empty(),
                class_bindings: EnvMap::empty(),
                class_stack: FlowVector::new(),
                scope_kind: ScopeKind::Global,
                ast_hint_map: HintMap::new(),
                hint_map: EnvMap::empty(),
                pred_func_map: FlowOrdMap::new(),
                name_defs: EnvMap::empty(),
            };
        }
        let mut env = CACHED.with(|cached| cached.clone());
        env.scope_kind = scope_kind;
        env
    }

    pub fn with_info(
        scope_kind: ScopeKind,
        ast_hint_map: HintMap,
        hint_map: EnvMap<ALoc, LazyHintT>,
        var_info: Rc<EnvInfo<ALoc>>,
        pred_func_map: FlowOrdMap<
            ALoc,
            Rc<Lazy<PredFuncallInfo, Box<dyn FnOnce() -> PredFuncallInfo>>>,
        >,
        name_defs: EnvEntriesMap,
    ) -> Self {
        let mut env = Self::empty(scope_kind);
        env.ast_hint_map = ast_hint_map;
        env.hint_map = hint_map;
        env.var_info = var_info;
        env.pred_func_map = pred_func_map;
        env.name_defs = name_defs;
        env
    }
}
