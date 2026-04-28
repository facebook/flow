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

pub struct TypeEntry<'a, CX = ()> {
    pub t: Type,
    pub state: Rc<RefCell<flow_lazy::Lazy<CX, Type, Box<dyn FnOnce(&CX) -> Type + 'a>>>>,
}

impl<CX> Clone for TypeEntry<'_, CX> {
    fn clone(&self) -> Self {
        TypeEntry {
            t: self.t.clone(),
            state: self.state.clone(),
        }
    }
}

pub struct LocEnv<'a, CX = ()> {
    pub types: EnvMap<ALoc, TypeEntry<'a, CX>>,
    pub tparams: EnvMap<ALoc, (SubstName, TypeParam, Type)>,
    pub class_bindings: EnvMap<ALoc, ClassBinding>,
    pub class_stack: FlowVector<ALoc>,
    pub scope_kind: ScopeKind,
    pub ast_hint_map: HintMap,
    pub hint_map: EnvMap<ALoc, LazyHintT<CX>>,
    pub var_info: Rc<EnvInfo<ALoc>>,
    pub pred_func_map: FlowOrdMap<
        ALoc,
        Rc<
            flow_lazy::Lazy<
                CX,
                Result<PredFuncallInfo, flow_utils_concurrency::job_error::JobError>,
                Box<
                    dyn FnOnce(
                            &CX,
                        ) -> Result<
                            PredFuncallInfo,
                            flow_utils_concurrency::job_error::JobError,
                        > + 'a,
                >,
            >,
        >,
    >,
    pub name_defs: EnvEntriesMap,
}

impl<CX> Clone for LocEnv<'_, CX> {
    fn clone(&self) -> Self {
        LocEnv {
            types: self.types.clone(),
            tparams: self.tparams.clone(),
            class_bindings: self.class_bindings.clone(),
            class_stack: self.class_stack.clone(),
            scope_kind: self.scope_kind.clone(),
            ast_hint_map: self.ast_hint_map.clone(),
            hint_map: self.hint_map.clone(),
            var_info: self.var_info.clone(),
            pred_func_map: self.pred_func_map.clone(),
            name_defs: self.name_defs.clone(),
        }
    }
}

impl<'a, CX> LocEnv<'a, CX> {
    pub fn initialize(&mut self, def_loc_kind: DefLocType, loc: ALoc, state: TypeEntry<'a, CX>) {
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

    pub fn find_write(&self, def_loc_kind: DefLocType, loc: ALoc) -> Option<&TypeEntry<'a, CX>> {
        self.types.get(&EnvKey::new(def_loc_kind, loc))
    }

    pub fn find_ordinary_write(&self, loc: ALoc) -> Option<&TypeEntry<'a, CX>> {
        self.find_write(DefLocType::OrdinaryNameLoc, loc)
    }

    pub fn empty(scope_kind: ScopeKind) -> Self {
        // Cache the non-generic fields in a thread_local to avoid expensive
        // im::OrdMap pool allocations (Arc::new) on every empty() call.
        // Fields that depend on CX/'a cannot be cached (different type params)
        // so they are constructed fresh.
        struct CachedFields {
            tparams: EnvMap<ALoc, (SubstName, TypeParam, Type)>,
            class_bindings: EnvMap<ALoc, ClassBinding>,
            class_stack: FlowVector<ALoc>,
            var_info: Rc<EnvInfo<ALoc>>,
            name_defs: EnvEntriesMap,
        }
        thread_local! {
            static CACHED: CachedFields = CachedFields {
                tparams: EnvMap::empty(),
                class_bindings: EnvMap::empty(),
                class_stack: FlowVector::new(),
                var_info: Rc::new(EnvInfo::empty()),
                name_defs: EnvMap::empty(),
            };
        }
        CACHED.with(|cached| LocEnv {
            types: EnvMap::empty(),
            tparams: cached.tparams.clone(),
            class_bindings: cached.class_bindings.clone(),
            class_stack: cached.class_stack.clone(),
            scope_kind,
            ast_hint_map: HintMap::new(),
            hint_map: EnvMap::empty(),
            var_info: cached.var_info.clone(),
            pred_func_map: FlowOrdMap::new(),
            name_defs: cached.name_defs.clone(),
        })
    }

    pub fn with_info(
        scope_kind: ScopeKind,
        ast_hint_map: HintMap,
        hint_map: EnvMap<ALoc, LazyHintT<CX>>,
        var_info: Rc<EnvInfo<ALoc>>,
        pred_func_map: FlowOrdMap<
            ALoc,
            Rc<
                flow_lazy::Lazy<
                    CX,
                    Result<PredFuncallInfo, flow_utils_concurrency::job_error::JobError>,
                    Box<
                        dyn FnOnce(
                                &CX,
                            ) -> Result<
                                PredFuncallInfo,
                                flow_utils_concurrency::job_error::JobError,
                            > + 'a,
                    >,
                >,
            >,
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
