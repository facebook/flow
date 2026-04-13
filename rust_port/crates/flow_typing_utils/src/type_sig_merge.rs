/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::LazyCell;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::rc::Weak;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_aloc::ALocTable;
use flow_aloc::aloc_representation_do_not_use;
use flow_common::flow_import_specifier::FlowImportSpecifier;
use flow_common::flow_import_specifier::Userland;
use flow_common::flow_symbol::Symbol;
use flow_common::polarity::Polarity;
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast_utils;
use flow_parser::file_key::FileKey;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;
use flow_type_sig::compact_table::Index;
use flow_type_sig::compact_table::Table;
use flow_type_sig::packed_type_sig;
use flow_type_sig::type_sig;
use flow_type_sig::type_sig::*;
use flow_type_sig::type_sig_pack as Pack;
use flow_typing_context::Context;
use flow_typing_context::ResolvedRequire;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::obj_type;
use flow_typing_type::type_;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::ModuleType;
use flow_typing_type::type_::ModuleTypeInner;
use flow_typing_type::type_::NamedSymbol;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::Type;
use flow_typing_type::type_util;
use vec1::Vec1;

use crate::annotation_inference;

type LazyModuleType<'cx> = Rc<
    flow_lazy::Lazy<Context<'cx>, ModuleType, Box<dyn FnOnce(&Context<'cx>) -> ModuleType + 'cx>>,
>;

pub type LazyExport<'cx> = Rc<
    flow_lazy::Lazy<Context<'cx>, NamedSymbol, Box<dyn FnOnce(&Context<'cx>) -> NamedSymbol + 'cx>>,
>;
pub type LazyCjsExport<'cx> = Rc<
    flow_lazy::Lazy<
        Context<'cx>,
        (Option<ALoc>, Type),
        Box<dyn FnOnce(&Context<'cx>) -> (Option<ALoc>, Type) + 'cx>,
    >,
>;

pub enum Exports<'cx> {
    CJSExports {
        type_exports: BTreeMap<FlowSmolStr, LazyExport<'cx>>,
        exports: Option<LazyCjsExport<'cx>>,
        type_stars: Vec<(ALoc, Index<FlowImportSpecifier>)>,
        strict: bool,
        platform_availability_set: Option<flow_common::platform_set::PlatformSet>,
    },
    ESExports {
        type_exports: BTreeMap<FlowSmolStr, LazyExport<'cx>>,
        exports: BTreeMap<FlowSmolStr, LazyExport<'cx>>,
        type_stars: Vec<(ALoc, Index<FlowImportSpecifier>)>,
        stars: Vec<(ALoc, Index<FlowImportSpecifier>)>,
        strict: bool,
        platform_availability_set: Option<flow_common::platform_set::PlatformSet>,
    },
}

#[derive(Clone, Dupe)]
pub struct File<'cx>(Rc<FileInner<'cx>>);

type DependenciesTable<'cx> = Table<(
    Userland,
    Rc<
        flow_lazy::Lazy<
            Context<'cx>,
            ResolvedRequire<'cx>,
            Box<dyn FnOnce(&Context<'cx>) -> ResolvedRequire<'cx> + 'cx>,
        >,
    >,
)>;

type LazyType<'cx> =
    Rc<flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>>;

type LocalDefsTable<'cx> = Table<
    Rc<
        flow_lazy::Lazy<
            Context<'cx>,
            (ALoc, FlowSmolStr, LazyType<'cx>, LazyType<'cx>),
            Box<
                dyn FnOnce(&Context<'cx>) -> (ALoc, FlowSmolStr, LazyType<'cx>, LazyType<'cx>)
                    + 'cx,
            >,
        >,
    >,
>;

type RemoteRefsTable<'cx> = Table<
    Rc<
        flow_lazy::Lazy<
            Context<'cx>,
            (ALoc, FlowSmolStr, Type),
            Box<dyn FnOnce(&Context<'cx>) -> (ALoc, FlowSmolStr, Type) + 'cx>,
        >,
    >,
>;

type PatternsTable<'cx> = Table<LazyType<'cx>>;

pub struct FileInner<'cx> {
    dependencies: RefCell<DependenciesTable<'cx>>,
    pub exports: Rc<dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<ModuleType, Type> + 'cx>,
    local_defs: RefCell<LocalDefsTable<'cx>>,
    remote_refs: RefCell<RemoteRefsTable<'cx>>,
    patterns: RefCell<PatternsTable<'cx>>,
    pattern_defs: RefCell<PatternsTable<'cx>>,
}

impl<'cx> std::ops::Deref for File<'cx> {
    type Target = FileInner<'cx>;
    fn deref(&self) -> &FileInner<'cx> {
        &self.0
    }
}

impl<'cx> File<'cx> {
    pub fn new(
        dependencies: DependenciesTable<'cx>,
        exports: Rc<dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<ModuleType, Type> + 'cx>,
        local_defs: LocalDefsTable<'cx>,
        remote_refs: RemoteRefsTable<'cx>,
        pattern_defs: PatternsTable<'cx>,
        patterns: PatternsTable<'cx>,
    ) -> Self {
        File(Rc::new(FileInner {
            dependencies: RefCell::new(dependencies),
            exports,
            local_defs: RefCell::new(local_defs),
            remote_refs: RefCell::new(remote_refs),
            pattern_defs: RefCell::new(pattern_defs),
            patterns: RefCell::new(patterns),
        }))
    }

    /// Break Rc reference cycles by clearing closure-holding fields.
    /// The `dependencies` Lazy closures capture `Arc<SharedMem>` and
    /// `Rc<CheckCache>`, forming cycles that prevent deallocation.
    /// Other closure fields (`local_defs`, `remote_refs`, etc.) capture
    /// `file_cell` (Weak — safe) and `cx.dupe()` which also forms cycles.
    pub fn drop_closures(&self) {
        *self.0.dependencies.borrow_mut() = Table::empty();
        *self.0.local_defs.borrow_mut() = Table::empty();
        *self.0.remote_refs.borrow_mut() = Table::empty();
        *self.0.patterns.borrow_mut() = Table::empty();
        *self.0.pattern_defs.borrow_mut() = Table::empty();
    }

    /// Create a Weak reference to the inner FileInner.
    /// Used in file_cell to break self-referential Rc cycles:
    /// File<'cx> → Lazy closures → file_cell → File<'cx>
    pub fn downgrade(&self) -> Weak<FileInner<'cx>> {
        Rc::downgrade(&self.0)
    }

    /// Reconstruct a File<'cx> from a Weak reference stored in file_cell.
    /// The Weak upgrade is safe because this is only called from Lazy closures
    /// stored within the File<'cx> itself — the File<'cx> must be alive for the closure
    /// to be called.
    pub fn from_weak(weak: &Weak<FileInner<'cx>>) -> Self {
        File(
            weak.upgrade()
                .expect("type_sig_merge::File dropped before lazy closure executed"),
        )
    }
}

pub type TparamsMap = FlowOrdMap<FlowSmolStr, Type>;

pub fn def_reason<T>(def: &Def<ALoc, T>) -> Reason {
    use flow_common::reason::VirtualReasonDesc::*;
    match def {
        Def::TypeAlias(_) | Def::OpaqueType(_) => {
            type_::desc_format::type_reason(Name::new(def.name().dupe()), def.id_loc())
        }
        Def::Interface(_)
        | Def::ClassBinding(_)
        | Def::DeclareClassBinding(_)
        | Def::RecordBinding(_)
        | Def::DisabledRecordBinding(_) => {
            type_::desc_format::instance_reason(Name::new(def.name().dupe()), def.id_loc())
        }
        Def::FunBinding(inner) => {
            reason::func_reason(inner.async_, inner.generator, inner.fn_loc.dupe())
        }
        Def::DeclareFun(_) => reason::mk_reason(RFunctionType, def.id_loc()),
        Def::ComponentBinding(inner) => reason::mk_reason(
            RComponent(Name::new(inner.name.dupe())),
            inner.fn_loc.dupe(),
        ),
        Def::DisabledComponentBinding(inner) => reason::mk_reason(
            RIdentifier(Name::new(inner.name.dupe())),
            inner.id_loc.dupe(),
        ),
        Def::Variable(_) | Def::Parameter(_) => {
            reason::mk_reason(RIdentifier(Name::new(def.name().dupe())), def.id_loc())
        }
        Def::DisabledEnumBinding(_) | Def::EnumBinding(_) => reason::mk_reason(
            REnum {
                name: Some(def.name().dupe()),
            },
            def.id_loc(),
        ),
        Def::NamespaceBinding(_) => reason::mk_reason(RNamespace(def.name().dupe()), def.id_loc()),
    }
}

pub fn remote_ref_reason(remote_ref: &Pack::RemoteRef<ALoc>) -> Reason {
    use flow_common::reason::VirtualReasonDesc::*;
    match remote_ref {
        Pack::RemoteRef::Import { id_loc, name, .. }
        | Pack::RemoteRef::ImportNs { id_loc, name, .. } => {
            reason::mk_reason(RIdentifier(Name::new(name.dupe())), id_loc.dupe())
        }
        Pack::RemoteRef::ImportType { id_loc, name, .. }
        | Pack::RemoteRef::ImportTypeof { id_loc, name, .. }
        | Pack::RemoteRef::ImportTypeofNs { id_loc, name, .. }
        | Pack::RemoteRef::ImportTypeNs { id_loc, name, .. } => {
            type_::desc_format::type_reason(Name::new(name.dupe()), id_loc.dupe())
        }
    }
}

fn eval_id_of_aloc<'cx>(cx: &Context<'cx>, loc: ALoc) -> type_::eval::Id {
    type_::eval::Id::of_aloc_id(true, cx.make_aloc_id(&loc))
}

fn specialize<'cx>(cx: &Context<'cx>, reason_op: Reason, t: Type) -> Type {
    let reason = type_util::reason_of_t(&t).dupe();
    annotation_inference::specialize(cx, t, type_::unknown_use(), reason_op, reason, None)
}

/// Repositioning the underlying type does not seem to have any perceptible impact
/// when dealing with annotations. Instead of invoking the convoluted Flow_js.reposition
/// implementation here, we just return the type intact. What does have an effect is the
/// lazy tvar indirection, which updates the reason on the new OpenT.
fn reposition_sig_tvar<'cx>(cx: &Context<'cx>, loc: ALoc, t: Type) -> Type {
    let reason = type_util::reason_of_t(&t).dupe().reposition(loc);
    annotation_inference::mk_sig_tvar(cx, reason, Rc::new(flow_lazy::Lazy::new_forced(t)))
}

fn eval_arith<'cx>(
    cx: &Context<'cx>,
    loc: ALoc,
    lhs_t: Type,
    rhs_t: Type,
    op: flow_parser::ast::expression::BinaryOperator,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    let desc = RBinaryOperator(Box::new((
        ast_utils::string_of_binary_operator(op).into(),
        Arc::new(type_util::reason_of_t(&lhs_t).desc(true).clone()),
        Arc::new(type_util::reason_of_t(&rhs_t).desc(true).clone()),
    )));
    let reason = reason::mk_reason(desc, loc);
    let kind = type_::arith_kind::ArithKind::of_binary_operator(op);
    annotation_inference::arith(cx, reason, lhs_t, rhs_t, kind)
}

fn eval_unary<'cx>(
    cx: &Context<'cx>,
    loc: ALoc,
    t: Type,
    op: flow_parser::ast::expression::UnaryOperator,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_parser::ast::expression::UnaryOperator;
    match op {
        UnaryOperator::Minus => {
            let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
            annotation_inference::unary_arith(cx, reason, t, type_::UnaryArithKind::Minus)
        }
        UnaryOperator::Plus => {
            let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
            annotation_inference::unary_arith(cx, reason, t, type_::UnaryArithKind::Plus)
        }
        UnaryOperator::BitNot => {
            let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
            annotation_inference::unary_arith(cx, reason, t, type_::UnaryArithKind::BitNot)
        }
        UnaryOperator::Not => {
            let reason = reason::mk_reason(
                RUnaryOperator("not".into(), Arc::new(type_util::desc_of_t(&t).clone())),
                loc,
            );
            annotation_inference::unary_not(cx, reason, t)
        }
        UnaryOperator::Typeof => type_::str_module_t::at(loc),
        UnaryOperator::Void => type_::void::at(loc),
        UnaryOperator::Delete => type_::bool_module_t::at(loc),
        // This is a parse error
        UnaryOperator::Nonnull | UnaryOperator::Await => {
            type_::any_t::at(type_::AnySource::AnyError(None), loc)
        }
    }
}

fn eval_update<'cx>(cx: &Context<'cx>, loc: ALoc, t: Type) -> Type {
    let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
    annotation_inference::unary_arith(cx, reason, t, type_::UnaryArithKind::Update)
}

fn eval<'cx>(cx: &Context<'cx>, loc: ALoc, t: Type, op: Op<Type>) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    match op {
        Op::Arith(bin_op, rhs_t) => eval_arith(cx, loc, t, rhs_t, bin_op),
        Op::Unary(unary_op) => eval_unary(cx, loc, t, unary_op),
        Op::Update => eval_update(cx, loc, t),
        Op::GetProp(name) => {
            let name = Name::new(name);
            let reason = reason::mk_reason(RProperty(Some(name.dupe())), loc);
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_prop(cx, use_op, reason, None, name, t)
        }
        Op::GetElem(index) => {
            let reason = reason::mk_reason(RProperty(None), loc);
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_elem(cx, use_op, reason, index, t)
        }
    }
}

fn async_void_return<'cx>(cx: &Context<'cx>, loc: ALoc) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    let reason = reason::mk_reason(RAsyncReturn, loc.dupe());
    let targs = vec![flow_typing_type::type_::void::at(loc)];
    flow_js_utils::lookup_builtin_typeapp(cx, reason, "Promise", targs)
}

fn add_default_constructor<T>(
    reason: Reason,
    extends: &ClassExtends<ALoc, T>,
    props: &mut BTreeMap<FlowSmolStr, type_::Property>,
) {
    match extends {
        ClassExtends::ClassExplicitExtends { .. }
        | ClassExtends::ClassExplicitExtendsApp { .. } => {}
        ClassExtends::ClassImplicitExtends | ClassExtends::ObjectPrototypeExtendsNull => {
            let key = FlowSmolStr::new_inline("constructor");
            if props.contains_key(&key) {
                return;
            }
            let reason = reason.replace_desc(reason::VirtualReasonDesc::RDefaultConstructor);
            let return_t = type_::void::why(reason.dupe());
            let statics = type_::dummy_static(reason.dupe());
            let funtype = type_::mk_boundfunctiontype(
                type_::implicit_mixed_this(reason.dupe()),
                None,
                vec![],
                None,
                reason.dupe(),
                None,
                None,
                return_t,
            );
            let type_ = Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::FunT(statics, Rc::new(funtype))),
            ));
            props.insert(
                key,
                type_::Property::new(type_::PropertyInner::Method {
                    key_loc: None,
                    type_,
                }),
            );
        }
    }
}

fn add_record_constructor<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    name: &FlowSmolStr,
    own_props: &BTreeMap<FlowSmolStr, type_::Property>,
    defaulted_props: &flow_data_structure_wrapper::ord_set::FlowOrdSet<FlowSmolStr>,
    class_props: &mut BTreeMap<FlowSmolStr, type_::Property>,
) {
    use flow_common::reason::VirtualReasonDesc::*;

    let record_reason = reason.dupe().replace_desc(RRecordType(name.dupe()));
    let return_t = type_::void::why(reason.dupe());
    let statics = type_::dummy_static(reason.dupe());
    let props: type_::properties::PropertiesMap = own_props
        .iter()
        .map(|(prop_name, prop)| {
            let new_prop = match &**prop {
                type_::PropertyInner::Field(fd) => {
                    if defaulted_props.contains(prop_name) {
                        let r = type_util::reason_of_t(&fd.type_).dupe();
                        let optional_reason = r.dupe().update_desc_new(|desc| {
                            reason::VirtualReasonDesc::ROptional(Arc::new(desc))
                        });
                        type_::Property::new(type_::PropertyInner::Field(Box::new(
                            type_::FieldData {
                                preferred_def_locs: fd.preferred_def_locs.clone(),
                                key_loc: fd.key_loc.dupe(),
                                type_: Type::new(type_::TypeInner::OptionalT {
                                    reason: optional_reason,
                                    type_: fd.type_.dupe(),
                                    use_desc: false,
                                }),
                                polarity: fd.polarity.clone(),
                            },
                        )))
                    } else {
                        prop.dupe()
                    }
                }
                _ => prop.dupe(),
            };
            (Name::new(prop_name.dupe()), new_prop)
        })
        .collect();
    let param = obj_type::mk_with_proto(
        cx,
        record_reason.dupe(),
        type_::ObjKind::Exact,
        None,
        None,
        Some(props),
        None,
        Type::new(type_::TypeInner::NullProtoT(record_reason)),
    );
    let funtype = type_::mk_boundfunctiontype(
        type_::implicit_mixed_this(reason.dupe()),
        None,
        vec![param],
        None,
        reason.dupe(),
        None,
        None,
        return_t,
    );
    let constructor = type_::Property::new(type_::PropertyInner::Method {
        key_loc: None,
        type_: Type::new(type_::TypeInner::DefT(
            reason,
            type_::DefT::new(type_::DefTInner::FunT(statics, Rc::new(funtype))),
        )),
    });
    class_props.insert(FlowSmolStr::new_inline("constructor"), constructor);
}

fn add_name_field(reason: Reason, props: &mut BTreeMap<Name, type_::Property>) {
    let name_key = Name::new(FlowSmolStr::new("name"));
    props.entry(name_key).or_insert_with(|| {
        type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
            preferred_def_locs: None,
            key_loc: None,
            type_: type_::str_module_t::why(reason),
            polarity: Polarity::Neutral,
        })))
    });
}

fn require<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    loc: ALoc,
    index: Index<FlowImportSpecifier>,
    standard_cjs_esm_interop: bool,
) -> Type {
    let (mref, lazy_resolved) = {
        let deps = file.dependencies.borrow();
        let (m, l) = deps.get(index);
        (m.dupe(), l.dupe())
    };
    let resolved_require = lazy_resolved.get_forced(cx).dupe();
    let reason = reason::mk_reason(reason::VirtualReasonDesc::RModule(mref.dupe()), loc.dupe());
    let symbol = Symbol::mk_module_symbol(mref.dupe().into_inner(), loc.dupe());
    annotation_inference::cjs_require(
        cx,
        reason,
        symbol,
        false,
        standard_cjs_esm_interop,
        resolved_require,
    )
}

fn import<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    index: Index<FlowImportSpecifier>,
    kind: type_::ImportKind,
    remote: &FlowSmolStr,
    local: &FlowSmolStr,
) -> Type {
    let (mref, lazy_resolved) = {
        let deps = file.dependencies.borrow();
        let (m, l) = deps.get(index);
        (m.dupe(), l.dupe())
    };
    let resolved_require = lazy_resolved.get_forced(cx).dupe();
    if remote.as_str() == "default" {
        annotation_inference::import_default(
            cx,
            reason,
            kind,
            local,
            mref.dupe(),
            false,
            resolved_require,
        )
    } else {
        annotation_inference::import_named(
            cx,
            reason,
            kind,
            remote,
            mref.dupe(),
            false,
            resolved_require,
        )
    }
}

fn import_ns<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    name: &FlowSmolStr,
    id_loc: ALoc,
    index: Index<FlowImportSpecifier>,
) -> Type {
    let lazy_resolved = file.dependencies.borrow().get(index).1.dupe();
    let resolved_require = lazy_resolved.get_forced(cx).dupe();
    let namespace_symbol = Symbol::mk_module_symbol(
        Userland::from_smol_str(name.dupe()).into_inner(),
        id_loc.dupe(),
    );
    annotation_inference::import_ns(cx, reason, namespace_symbol, false, resolved_require)
}

fn import_typeof_ns<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    name: &FlowSmolStr,
    id_loc: ALoc,
    index: Index<FlowImportSpecifier>,
) -> Type {
    let lazy_resolved = file.dependencies.borrow().get(index).1.dupe();
    let resolved_require = lazy_resolved.get_forced(cx).dupe();
    let namespace_symbol = Symbol::mk_namespace_symbol(name.dupe(), id_loc.dupe());
    let ns_t = annotation_inference::import_ns(
        cx,
        reason.dupe(),
        namespace_symbol,
        false,
        resolved_require,
    );
    annotation_inference::import_typeof(cx, reason, "*", ns_t)
}

fn merge_enum<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    id_loc: ALoc,
    enum_name: &FlowSmolStr,
    rep: &EnumRep,
    members: &BTreeMap<FlowSmolStr, ALoc>,
    has_unknown_members: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let rep_t = |desc, def_t_inner| {
        let reason = reason::mk_reason(desc, id_loc.dupe());
        Type::new(type_::TypeInner::DefT(
            reason,
            type_::DefT::new(def_t_inner),
        ))
    };
    let representation_t = match rep {
        EnumRep::BoolRep(None) => rep_t(RBoolean, type_::DefTInner::BoolGeneralT),
        EnumRep::BoolRep(Some(lit)) => rep_t(
            RBoolean,
            type_::DefTInner::SingletonBoolT {
                value: *lit,
                from_annot: false,
            },
        ),
        EnumRep::NumberRep { truthy } => {
            let lit = if *truthy {
                type_::Literal::Truthy
            } else {
                type_::Literal::AnyLiteral
            };
            rep_t(RNumber, type_::DefTInner::NumGeneralT(lit))
        }
        EnumRep::StringRep { truthy } => {
            let lit = if *truthy {
                type_::Literal::Truthy
            } else {
                type_::Literal::AnyLiteral
            };
            rep_t(RString, type_::DefTInner::StrGeneralT(lit))
        }
        EnumRep::SymbolRep => rep_t(RSymbol, type_::DefTInner::SymbolT),
        EnumRep::BigIntRep { truthy } => {
            let lit = if *truthy {
                type_::Literal::Truthy
            } else {
                type_::Literal::AnyLiteral
            };
            rep_t(RBigInt, type_::DefTInner::BigIntGeneralT(lit))
        }
    };
    let enum_id = cx.make_aloc_id(&id_loc);
    let enum_info = type_::EnumInfo::new(type_::EnumInfoInner::ConcreteEnum(
        type_::EnumConcreteInfo::new(type_::EnumConcreteInfoInner {
            enum_name: enum_name.dupe(),
            enum_id,
            members: members.iter().map(|(k, v)| (k.dupe(), v.dupe())).collect(),
            representation_t,
            has_unknown_members,
        }),
    ));
    type_::mk_enum_object_type(reason, Rc::new(enum_info))
}

pub fn merge_pattern<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    pattern: &Pack::Pattern<ALoc>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    match pattern {
        Pack::Pattern::PDef(i) => {
            let __rc = file.pattern_defs.borrow().get(*i).dupe();
            __rc.get_forced(cx).dupe()
        }
        Pack::Pattern::PropP { id_loc, name, def } => {
            let t = {
                let __rc = file.patterns.borrow().get(*def).dupe();
                __rc.get_forced(cx).dupe()
            };
            let name = Name::new(name.dupe());
            let reason = reason::mk_reason(RProperty(Some(name.dupe())), id_loc.dupe());
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_prop(cx, use_op, reason, None, name, t)
        }
        Pack::Pattern::ComputedP { elem, def } => {
            let elem = {
                let __rc = file.pattern_defs.borrow().get(*elem).dupe();
                __rc.get_forced(cx).dupe()
            };
            let t = {
                let __rc = file.patterns.borrow().get(*def).dupe();
                __rc.get_forced(cx).dupe()
            };
            let loc = type_util::loc_of_t(&elem);
            let reason = reason::mk_reason(RProperty(None), loc.dupe());
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_elem(cx, use_op, reason, elem, t)
        }
        Pack::Pattern::UnsupportedLiteralP(loc) => {
            type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe())
        }
        Pack::Pattern::ObjRestP { loc, xs, def } => {
            let t = {
                let __rc = file.patterns.borrow().get(*def).dupe();
                __rc.get_forced(cx).dupe()
            };
            let reason = reason::mk_reason(RObjectPatternRestProp, loc.dupe());
            annotation_inference::obj_rest(cx, reason, xs.iter().duped().collect(), t)
        }
        Pack::Pattern::IndexP { loc, i, def } => {
            let t = {
                let __rc = file.patterns.borrow().get(*def).dupe();
                __rc.get_forced(cx).dupe()
            };
            let reason = reason::mk_reason(RArrayNthElement(*i as i32), loc.dupe());
            let i = {
                let reason = reason::mk_reason(RNumber, loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::SingletonNumT {
                        value: type_::NumberLiteral(*i as f64, FlowSmolStr::new(i.to_string())),
                        from_annot: false,
                    }),
                ))
            };
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_elem(cx, use_op, reason, i, t.dupe())
        }
        Pack::Pattern::ArrRestP { loc, i, def } => {
            let t = {
                let __rc = file.patterns.borrow().get(*def).dupe();
                __rc.get_forced(cx).dupe()
            };
            let reason = reason::mk_reason(RArrayPatternRestProp, loc.dupe());
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::arr_rest(cx, use_op, reason, *i as i32, t)
        }
    }
}

pub fn merge_remote_ref<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    remote_ref: &Pack::RemoteRef<ALoc>,
) -> Type {
    match remote_ref {
        Pack::RemoteRef::Import {
            id_loc: _,
            name,
            index,
            remote,
        } => import(
            cx,
            file,
            reason,
            index.clone(),
            type_::ImportKind::ImportValue,
            remote,
            name,
        ),
        Pack::RemoteRef::ImportType {
            id_loc: _,
            name,
            index,
            remote,
        } => import(
            cx,
            file,
            reason,
            index.clone(),
            type_::ImportKind::ImportType,
            remote,
            name,
        ),
        Pack::RemoteRef::ImportTypeof {
            id_loc: _,
            name,
            index,
            remote,
        } => import(
            cx,
            file,
            reason,
            index.clone(),
            type_::ImportKind::ImportTypeof,
            remote,
            name,
        ),
        Pack::RemoteRef::ImportNs {
            id_loc,
            name,
            index,
        } => import_ns(cx, file, reason, name, id_loc.dupe(), *index),
        Pack::RemoteRef::ImportTypeofNs {
            id_loc,
            name,
            index,
        } => import_typeof_ns(cx, file, reason, name, id_loc.dupe(), *index),
        Pack::RemoteRef::ImportTypeNs {
            id_loc,
            name,
            index,
        } => import_ns(cx, file, reason, name, id_loc.dupe(), *index),
    }
}

fn merge_ref<'cx, R>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    f: impl FnOnce(Type, ALoc, ALoc, &FlowSmolStr) -> R,
    packed_ref: &Pack::PackedRef<ALoc>,
    const_decl: bool,
) -> R {
    use flow_common::reason::VirtualReasonDesc::*;

    match packed_ref {
        Pack::PackedRef::LocalRef { ref_loc, index } => {
            let __rc = file.local_defs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (def_loc, name, t_general, t_const) = entry;
            let t = if const_decl { t_const } else { t_general }
                .get_forced(cx)
                .dupe();
            let t = reposition_sig_tvar(cx, ref_loc.dupe(), t);
            f(t, ref_loc.dupe(), def_loc.dupe(), name)
        }
        Pack::PackedRef::RemoteRef { ref_loc, index } => {
            let __rc = file.remote_refs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (def_loc, name, t) = entry;
            let t = reposition_sig_tvar(cx, ref_loc.dupe(), t.dupe());
            f(t, ref_loc.dupe(), def_loc.dupe(), name)
        }
        Pack::PackedRef::BuiltinRef {
            ref_loc,
            type_ref,
            name,
        } => {
            let reason = reason::mk_reason(RIdentifier(Name::new(name.dupe())), ref_loc.dupe());
            let t = if *type_ref {
                flow_js_utils::lookup_builtin_type(cx, name.as_str(), reason)
            } else {
                flow_js_utils::lookup_builtin_value(cx, name.as_str(), reason)
            };
            let def_loc = type_util::reason_of_t(&t).def_loc().dupe();
            f(t, ref_loc.dupe(), def_loc, name)
        }
    }
}

fn merge_tyref<'cx, R>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    f: impl FnOnce(Type, ALoc, Vec<FlowSmolStr>) -> R,
    tyref: &Pack::TyRef<ALoc>,
) -> R {
    use flow_common::reason::VirtualReasonDesc::*;

    let mut qualifications = Vec::new();
    let mut current = tyref;
    loop {
        match current {
            Pack::TyRef::Unqualified(packed_ref) => {
                let (mut t, mut loc, mut names) = merge_ref(
                    cx,
                    file,
                    |t, ref_loc, _def_loc, name| (t, ref_loc, vec![name.dupe()]),
                    packed_ref,
                    false,
                );
                for (q_loc, q_id_loc, q_name) in qualifications.iter().rev() {
                    let q_loc: &ALoc = q_loc;
                    let q_id_loc: &ALoc = q_id_loc;
                    let q_name: &FlowSmolStr = q_name;
                    names.push(q_name.dupe());
                    let qname: String = names
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(".");
                    let name = Name::new(q_name.dupe());
                    let id_reason = reason::mk_reason(RType(name.dupe()), q_id_loc.dupe());
                    let op_reason =
                        reason::mk_reason(RType(Name::new(FlowSmolStr::new(&qname))), q_loc.dupe());
                    let use_op = type_::VirtualUseOp::Op(Arc::new(
                        type_::VirtualRootUseOp::GetProperty(op_reason.dupe()),
                    ));
                    t = annotation_inference::qualify_type(
                        cx, use_op, id_reason, op_reason, name, t,
                    );
                    loc = q_loc.dupe();
                }
                names.reverse();
                return f(t, loc, names);
            }
            Pack::TyRef::Qualified {
                loc,
                id_loc,
                name,
                qualification,
            } => {
                qualifications.push((loc.dupe(), id_loc.dupe(), name.dupe()));
                current = qualification;
            }
        }
    }
}

pub fn merge_type_export<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    type_export: &Pack::TypeExport<ALoc>,
) -> NamedSymbol {
    match type_export {
        Pack::TypeExport::ExportTypeRef(ref_) => {
            let reason = reason.dupe();
            merge_ref(
                cx,
                file,
                |t, _ref_loc, def_loc, name| {
                    let type_ =
                        annotation_inference::assert_export_is_type(cx, reason, name.as_str(), t);
                    NamedSymbol::new(Some(def_loc), None, type_)
                },
                ref_,
                false,
            )
        }
        Pack::TypeExport::ExportTypeBinding(index) => {
            let __rc = file.local_defs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (loc, name, t_general, _t_const) = entry;
            let t = t_general.get_forced(cx).dupe();
            let type_ = annotation_inference::assert_export_is_type(cx, reason, name.as_str(), t);
            NamedSymbol::new(Some(loc.dupe()), None, type_)
        }
        Pack::TypeExport::ExportTypeFrom(index) => {
            let __rc = file.remote_refs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (loc, _name, type_) = entry;
            NamedSymbol::new(Some(loc.dupe()), None, type_.dupe())
        }
    }
}

fn mk_commonjs_module_t<'cx>(
    cx: &Context<'cx>,
    module_reason: Reason,
    module_is_strict: bool,
    module_available_platforms: Option<flow_common::platform_set::PlatformSet>,
    def_loc: Option<ALoc>,
    t: Type,
) -> LazyModuleType<'cx> {
    let module_export_types = type_::ExportTypes {
        value_exports_tmap: cx.make_export_map(type_::exports::T::new()),
        type_exports_tmap: cx.make_export_map(type_::exports::T::new()),
        cjs_export: Some((def_loc, t.dupe())),
        has_every_named_export: false,
    };
    let local_module = ModuleType::new(ModuleTypeInner {
        module_reason: module_reason.dupe(),
        module_export_types,
        module_is_strict,
        module_available_platforms,
    });
    annotation_inference::lazy_cjs_extract_named_exports(module_reason, local_module, t)
}

pub fn merge_exports<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    exports: Exports<'cx>,
) -> LazyModuleType<'cx> {
    type FromNs<'a> =
        Option<Rc<dyn Fn(&Context<'a>, &Context<'a>) -> Result<ModuleType, Type> + 'a>>;

    fn merge_star<'cx>(
        cx: &Context<'cx>,
        file: &File<'cx>,
        (loc, index): &(ALoc, Index<FlowImportSpecifier>),
    ) -> (ALoc, FromNs<'cx>) {
        let lazy_resolved = file.dependencies.borrow().get(index.clone()).1.dupe();
        let resolved_require = lazy_resolved.get_forced(cx).dupe();
        let f: FromNs<'cx> = match resolved_require {
            ResolvedRequire::TypedModule(f) => Some(f),
            ResolvedRequire::UncheckedModule(_) => None,
            ResolvedRequire::MissingModule => None,
        };
        (loc.dupe(), f)
    }

    fn mk_es_module_t<'cx>(
        cx: &Context<'cx>,
        module_reason: Reason,
        module_is_strict: bool,
        module_available_platforms: Option<flow_common::platform_set::PlatformSet>,
    ) -> ModuleType {
        let module_export_types = type_::ExportTypes {
            value_exports_tmap: cx.make_export_map(type_::exports::T::new()),
            type_exports_tmap: cx.make_export_map(type_::exports::T::new()),
            cjs_export: None,
            has_every_named_export: false,
        };
        ModuleType::new(ModuleTypeInner {
            module_reason,
            module_export_types,
            module_is_strict,
            module_available_platforms,
        })
    }

    fn copy_named_exports_star<'cx>(
        cx: &Context<'cx>,
        target_module_type: &ModuleType,
        (_, from_ns): &(ALoc, FromNs<'cx>),
    ) {
        match from_ns {
            None => (),
            Some(f) => {
                // Use merge_dst_cx to chain the destination context through
                // nested star exports (e.g., `export * from './cycle'`).
                let dst_cx = cx.merge_dst_cx().unwrap_or_else(|| cx.dupe());
                annotation_inference::copy_named_exports(cx, f(cx, &dst_cx), target_module_type);
            }
        }
    }

    fn copy_type_exports_star<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        target_module_type: &ModuleType,
        (loc, from_ns): &(ALoc, FromNs<'cx>),
    ) {
        match from_ns {
            None => (),
            Some(f) => {
                let dst_cx = cx.merge_dst_cx().unwrap_or_else(|| cx.dupe());
                let reason = reason.dupe().reposition(loc.dupe());
                annotation_inference::copy_type_exports(
                    cx,
                    f(cx, &dst_cx),
                    reason,
                    target_module_type,
                );
            }
        }
    }

    fn copy_star_exports<'cx>(
        cx: &Context<'cx>,
        reason: &Reason,
        xs: &[(ALoc, FromNs<'cx>)],
        ys: &[(ALoc, FromNs<'cx>)],
        target_module_type: &ModuleType,
    ) {
        // let rec loop file reason target_module_type = function
        let mut xs = xs;
        let mut ys = ys;
        loop {
            match (xs, ys) {
                ([], []) => break,
                (xs, []) => {
                    for x in xs {
                        copy_named_exports_star(cx, target_module_type, x);
                    }
                    break;
                }
                ([], ys) => {
                    for y in ys {
                        copy_type_exports_star(cx, reason, target_module_type, y);
                    }
                    break;
                }
                ([x, xs_ @ ..], [y, ys_ @ ..]) => {
                    if x.0.cmp(&y.0) == std::cmp::Ordering::Greater {
                        copy_named_exports_star(cx, target_module_type, x);
                        xs = xs_;
                    } else {
                        copy_type_exports_star(cx, reason, target_module_type, y);
                        ys = ys_;
                    }
                }
            }
        }
    }

    match exports {
        Exports::CJSExports {
            type_exports,
            exports,
            type_stars,
            strict,
            platform_availability_set,
        } => {
            let (def_loc_opt, exports_t) = match exports {
                // | Some (lazy (def_loc_opt, t)) -> (def_loc_opt, t)
                Some(lazy_val) => {
                    let (def_loc_opt, t) = lazy_val.get_forced(cx).clone();
                    (def_loc_opt, t)
                }
                None => (
                    None,
                    obj_type::mk_with_proto(
                        cx,
                        reason.dupe(),
                        type_::ObjKind::Exact,
                        None,
                        None,
                        None,
                        None,
                        Type::new(type_::TypeInner::ObjProtoT(reason.dupe())),
                    ),
                ),
            };
            // let type_exports = SMap.map Lazy.force type_exports |> NameUtils.namemap_of_smap in
            let type_exports_map: type_::exports::T = type_exports
                .iter()
                .map(|(key, lazy_val)| (Name::new(key.dupe()), lazy_val.get_forced(cx).clone()))
                .collect();
            let type_stars: Vec<(ALoc, FromNs<'cx>)> =
                type_stars.iter().map(|s| merge_star(cx, file, s)).collect();
            let reason2 = reason.dupe();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                let lazy_module = mk_commonjs_module_t(
                    cx,
                    reason2.dupe(),
                    strict,
                    platform_availability_set,
                    def_loc_opt,
                    exports_t,
                );
                let module_type = lazy_module.get_forced(cx).dupe();
                flow_js_utils::export_named_t_kit::mod_module_t(
                    cx,
                    type_::exports::T::new(),
                    type_exports_map,
                    type_::ExportKind::DirectExport,
                    &module_type,
                );
                copy_star_exports(cx, &reason2, &[], &type_stars, &module_type);
                module_type
            })))
        }
        Exports::ESExports {
            type_exports,
            exports,
            stars,
            type_stars,
            strict,
            platform_availability_set,
        } => {
            // let exports = SMap.map Lazy.force exports |> NameUtils.namemap_of_smap in
            let exports_map: type_::exports::T = exports
                .iter()
                .map(|(key, lazy_val)| (Name::new(key.dupe()), lazy_val.get_forced(cx).clone()))
                .collect();
            // let type_exports = SMap.map Lazy.force type_exports |> NameUtils.namemap_of_smap in
            let type_exports_map: type_::exports::T = type_exports
                .iter()
                .map(|(key, lazy_val)| (Name::new(key.dupe()), lazy_val.get_forced(cx).clone()))
                .collect();
            let stars: Vec<(ALoc, FromNs<'cx>)> =
                stars.iter().map(|s| merge_star(cx, file, s)).collect();
            let type_stars: Vec<(ALoc, FromNs<'cx>)> =
                type_stars.iter().map(|s| merge_star(cx, file, s)).collect();
            let reason2 = reason.dupe();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                let module_type =
                    mk_es_module_t(cx, reason2.dupe(), strict, platform_availability_set);
                flow_js_utils::export_named_t_kit::mod_module_t(
                    cx,
                    exports_map,
                    type_exports_map,
                    type_::ExportKind::DirectExport,
                    &module_type,
                );
                copy_star_exports(cx, &reason2, &stars, &type_stars, &module_type);
                module_type
            })))
        }
    }
}

#[derive(Clone, Dupe)]
struct MergeEnv {
    tps: FlowOrdMap<FlowSmolStr, Type>,
    infer_tps: FlowOrdMap<FlowSmolStr, (ALoc, Type)>,
    in_no_infer: bool,
    in_renders_arg: bool,
}

fn mk_merge_env(tps: FlowOrdMap<FlowSmolStr, Type>) -> MergeEnv {
    MergeEnv {
        tps,
        infer_tps: FlowOrdMap::new(),
        in_no_infer: false,
        in_renders_arg: false,
    }
}

fn merge_impl<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    packed: &Pack::Packed<ALoc>,
    as_const: bool,
    const_decl: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    match packed {
        Pack::Packed::Annot(t) => merge_annot(env, cx, file, t),
        Pack::Packed::Value(t) => merge_value(env, cx, file, t, as_const, const_decl),
        // Let's have the property retain the precise type in
        // export const FOO = "foo";
        // export const OBJ = { FOO } as const;
        Pack::Packed::Ref(ref_) => merge_ref(
            cx,
            file,
            |t, _ref_loc, _def_loc, _name| t,
            ref_,
            as_const || const_decl,
        ),
        Pack::Packed::TyRef(name) => {
            let in_renders_arg = env.in_renders_arg;
            merge_tyref(
                cx,
                file,
                |t, ref_loc, names| {
                    let (name, _) = (names.first().duped().unwrap_or_default(), &names[1..]);
                    let reason = reason::mk_annot_reason(RType(Name::new(name)), ref_loc);
                    let type_t_kind = if in_renders_arg {
                        type_::TypeTKind::RenderTypeKind
                    } else {
                        type_::TypeTKind::TypeAliasKind
                    };
                    annotation_inference::mk_type_reference(cx, type_t_kind, reason, t)
                },
                name,
            )
        }
        Pack::Packed::TyRefApp { loc, name, targs } => {
            let targs: Vec<Type> = targs
                .iter()
                .map(|t| merge_impl(env, cx, file, t, false, false))
                .collect();
            let loc = loc.dupe();
            merge_tyref(
                cx,
                file,
                move |t, _ref_loc, _names| type_util::typeapp_annot(false, false, loc, t, targs),
                name,
            )
        }
        Pack::Packed::AsyncVoidReturn(loc) => async_void_return(cx, loc.dupe()),
        Pack::Packed::Pattern(i) => {
            let __rc = file.patterns.borrow().get(*i).dupe();
            __rc.get_forced(cx).dupe()
        }
        Pack::Packed::Err(loc) => type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe()),
        Pack::Packed::Eval(loc, t, op) => {
            let (eval_as_const, eval_const_decl) = match op {
                Op::Unary(flow_parser::ast::expression::UnaryOperator::Minus)
                | Op::Unary(flow_parser::ast::expression::UnaryOperator::Not) => {
                    (as_const, const_decl)
                }
                _ => (false, false),
            };
            let merged_t = merge_impl(env, cx, file, t, eval_as_const, eval_const_decl);
            let merged_op = merge_op(env, cx, file, op);
            eval(cx, loc.dupe(), merged_t, merged_op)
        }
        Pack::Packed::Require { loc, index } => require(cx, file, loc.dupe(), *index, false),
        Pack::Packed::ImportDynamic { loc, index } => {
            let mref = file.dependencies.borrow().get(*index).0.dupe();
            let ns_reason = reason::mk_reason(RModule(mref.dupe()), loc.dupe());
            let name = mref.dupe().into_inner();
            let ns_t = import_ns(cx, file, ns_reason, &name, loc.dupe(), index.clone());
            let reason = reason::mk_annot_reason(RAsyncImport, loc.dupe());
            flow_js_utils::lookup_builtin_typeapp(cx, reason, "Promise", vec![ns_t])
        }
        Pack::Packed::ModuleRef { loc, index } => {
            let t = require(cx, file, loc.dupe(), *index, true);
            let reason = reason::mk_reason(RModuleReference, loc.dupe());
            flow_js_utils::lookup_builtin_typeapp(cx, reason, "$Flow$ModuleRef", vec![t])
        }
        Pack::Packed::ImportTypeAnnot { loc, index } => {
            let mref = file.dependencies.borrow().get(*index).0.dupe();
            let ns_reason = reason::mk_reason(RModule(mref.dupe()), loc.dupe());
            let name = mref.dupe().into_inner();
            import_ns(cx, file, ns_reason, &name, loc.dupe(), *index)
        }
    }
}

fn resolve_computed_key<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    key: &Pack::Packed<ALoc>,
) -> Option<Name> {
    let key_t = merge_impl(env, cx, file, key, false, false);
    match cx.find_resolved(&key_t) {
        Some(resolved_t) => match flow_js_utils::propref_for_elem_t(cx, &resolved_t) {
            type_::PropRef::Named { name, .. } => Some(name),
            type_::PropRef::Computed(_) => None,
        },
        None => None,
    }
}

fn merge_overloaded_property(
    existing_prop: &type_::Property,
    new_prop: type_::Property,
) -> type_::Property {
    match (&**existing_prop, &*new_prop) {
        (
            type_::PropertyInner::Method {
                type_: existing_t, ..
            },
            type_::PropertyInner::Method {
                key_loc,
                type_: new_t,
            },
        ) => {
            let reason = type_util::reason_of_t(new_t).dupe();
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: key_loc.dupe(),
                type_: Type::new(type_::TypeInner::IntersectionT(
                    reason,
                    type_::inter_rep::make(existing_t.dupe(), new_t.dupe(), vec![].into()),
                )),
            })
        }
        _ => new_prop,
    }
}

fn merge_annot<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    annot: &Pack::PackedAnnot<ALoc>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_type_sig::type_sig::Annot;
    match annot {
        Annot::Any(loc) => type_::any_t::at(type_::AnySource::AnnotatedAny, loc.dupe()),
        Annot::Mixed(loc) => type_::mixed_t::at(loc.dupe()),
        Annot::Empty(loc) => type_::empty_t::at(loc.dupe()),
        Annot::Void(loc) => type_::void::at(loc.dupe()),
        Annot::Null(loc) => type_::null::at(loc.dupe()),
        Annot::Symbol(loc) => type_::symbol_t::at(loc.dupe()),
        Annot::UniqueSymbol(loc) => type_::unique_symbol_t::at(cx.make_aloc_id(loc), loc.dupe()),
        Annot::Number(loc) => type_::num_module_t::at(loc.dupe()),
        Annot::BigInt(loc) => type_::bigint_module_t::at(loc.dupe()),
        Annot::String(loc) => type_::str_module_t::at(loc.dupe()),
        Annot::Boolean(loc) => type_::bool_module_t::at(loc.dupe()),
        Annot::Exists(loc) => type_::any_t::at(type_::AnySource::AnnotatedAny, loc.dupe()),
        Annot::Optional(t) => {
            let t = merge_impl(env, cx, file, t, false, false);
            type_util::optional(t, None, false)
        }
        Annot::Maybe(box (loc, t)) => {
            let t = merge_impl(env, cx, file, t, false, false);
            let desc = type_util::desc_of_t(&t).clone();
            let reason = reason::mk_annot_reason(RMaybe(Arc::new(desc)), loc.dupe());
            Type::new(type_::TypeInner::MaybeT(reason, t))
        }
        Annot::Union(inner) => {
            let AnnotUnion { loc, t0, t1, ts } = inner.as_ref();
            let reason = reason::mk_annot_reason(RUnionType, loc.dupe());
            let t0 = merge_impl(env, cx, file, t0, false, false);
            let t1 = merge_impl(env, cx, file, t1, false, false);
            let ts: Vec<Type> = ts
                .iter()
                .map(|t| merge_impl(env, cx, file, t, false, false))
                .collect();
            let source_aloc = Some(cx.make_aloc_id(loc));
            Type::new(type_::TypeInner::UnionT(
                reason,
                type_::union_rep::make(
                    source_aloc,
                    type_::union_rep::UnionKind::UnknownKind,
                    t0,
                    t1,
                    ts.into(),
                ),
            ))
        }
        Annot::Intersection(inner) => {
            let AnnotIntersection { loc, t0, t1, ts } = inner.as_ref();
            let reason = reason::mk_annot_reason(RIntersectionType, loc.dupe());
            let t0 = merge_impl(env, cx, file, t0, false, false);
            let t1 = merge_impl(env, cx, file, t1, false, false);
            let ts: Vec<Type> = ts
                .iter()
                .map(|t| merge_impl(env, cx, file, t, false, false))
                .collect();
            Type::new(type_::TypeInner::IntersectionT(
                reason,
                type_::inter_rep::make(t0, t1, ts.into()),
            ))
        }
        Annot::Tuple(inner) => {
            let AnnotTuple {
                loc,
                elems,
                inexact,
            } = inner.as_ref();
            let reason = reason::mk_annot_reason(RTupleType, loc.dupe());
            let unresolved: Vec<type_::UnresolvedParam> = elems
                .iter()
                .map(|elem: &type_sig::TupleElement<_, _>| match elem {
                    type_sig::TupleElement::TupleElement {
                        loc,
                        name,
                        t,
                        polarity,
                        optional,
                    } => {
                        let reason =
                            reason::mk_reason(RTupleElement { name: name.dupe() }, loc.dupe());
                        let t = merge_impl(env, cx, file, t, false, false);
                        let elem = type_::TupleElement {
                            name: name.dupe(),
                            t,
                            polarity: *polarity,
                            optional: *optional,
                            reason,
                        };
                        type_::UnresolvedParam::UnresolvedArg(elem, None)
                    }
                    type_sig::TupleElement::TupleSpread { loc: _, name: _, t } => {
                        let t = merge_impl(env, cx, file, t, false, false);
                        type_::UnresolvedParam::UnresolvedSpreadArg(t)
                    }
                })
                .collect();
            let mk_type_destructor = |_cx: &Context,
                                      use_op: type_::UseOp,
                                      reason: Reason,
                                      t: Type,
                                      destructor: type_::Destructor,
                                      id: type_::eval::Id|
             -> Result<Type, flow_js_utils::FlowJsException> {
                Ok(Type::new(type_::TypeInner::EvalT {
                    type_: t,
                    defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                        use_op,
                        reason,
                        Rc::new(destructor),
                    )),
                    id,
                }))
            };
            let id = eval_id_of_aloc(cx, loc.dupe());
            flow_js_utils::mk_tuple_type(cx, id, mk_type_destructor, *inexact, reason, unresolved)
                .unwrap()
        }
        Annot::Array(box (loc, t)) => {
            let reason = reason::mk_annot_reason(RArrayType, loc.dupe());
            let elem_t = merge_impl(env, cx, file, t, false, false);
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ArrayAT {
                    elem_t,
                    tuple_view: None,
                    react_dro: None,
                }))),
            ))
        }
        Annot::ReadOnlyArray(box (loc, t)) => {
            let reason = reason::mk_annot_reason(RROArrayType, loc.dupe());
            let t = merge_impl(env, cx, file, t, false, false);
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ROArrayAT(
                    t, None,
                )))),
            ))
        }
        Annot::SingletonString(box (loc, str)) => {
            let reason = reason::mk_annot_reason(RStringLit(Name::new(str.dupe())), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::SingletonStrT {
                    from_annot: true,
                    value: Name::new(str.dupe()),
                }),
            ))
        }
        Annot::SingletonNumber(box (loc, num, raw)) => {
            let reason = reason::mk_annot_reason(RNumberLit(raw.dupe()), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::SingletonNumT {
                    from_annot: true,
                    value: type_::NumberLiteral(*num, raw.dupe()),
                }),
            ))
        }
        Annot::SingletonBigInt(box (loc, bigint, raw)) => {
            let reason = reason::mk_annot_reason(RBigIntLit(raw.dupe()), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::SingletonBigIntT {
                    from_annot: true,
                    value: type_::BigIntLiteral(*bigint, raw.dupe()),
                }),
            ))
        }
        Annot::SingletonBoolean(loc, b) => {
            let reason = reason::mk_annot_reason(RBooleanLit(*b), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::SingletonBoolT {
                    from_annot: true,
                    value: *b,
                }),
            ))
        }
        Annot::StringPrefix(inner) => {
            let AnnotStringPrefix {
                loc,
                prefix,
                remainder,
            } = inner.as_ref();
            let reason = reason::mk_reason(
                RStringPrefix {
                    prefix: prefix.dupe(),
                },
                loc.dupe(),
            );
            let remainder = remainder
                .as_ref()
                .map(|t| merge_impl(env, cx, file, t, false, false));
            Type::new(type_::TypeInner::StrUtilT {
                reason,
                op: type_::StrUtilOp::StrPrefix(prefix.dupe()),
                remainder,
            })
        }
        Annot::StringSuffix(inner) => {
            let AnnotStringSuffix {
                loc,
                suffix,
                remainder,
            } = inner.as_ref();
            let reason = reason::mk_reason(
                RStringSuffix {
                    suffix: suffix.dupe(),
                },
                loc.dupe(),
            );
            let remainder = remainder
                .as_ref()
                .map(|t| merge_impl(env, cx, file, t, false, false));
            Type::new(type_::TypeInner::StrUtilT {
                reason,
                op: type_::StrUtilOp::StrSuffix(suffix.dupe()),
                remainder,
            })
        }
        Annot::Typeof(inner) => {
            let AnnotTypeof {
                loc,
                qname,
                t,
                targs,
            } = inner.as_ref();
            let qname_str: FlowSmolStr = qname.join(".").into();
            let reason = reason::mk_reason(RTypeof(qname_str), loc.dupe());
            let t = merge_impl(env, cx, file, t, false, true);
            let targs = targs.as_ref().map(|ts| {
                ts.iter()
                    .map(|t| merge_impl(env, cx, file, t, false, false))
                    .collect()
            });
            type_util::typeof_annotation(reason, t, targs)
        }
        Annot::Bound(inner) => {
            let AnnotBound { ref_loc, name } = inner.as_ref();
            let t = match env.infer_tps.get(name) {
                Some((name_loc, t)) if name_loc == ref_loc => t.dupe(),
                _ => {
                    let t = env
                        .tps
                        .get(name)
                        .unwrap_or_else(|| {
                            panic!("merge_annot Bound: name '{}' not found in tps", name)
                        })
                        .dupe();
                    if env.in_no_infer {
                        match &*t {
                            type_::TypeInner::GenericT(box GenericTData {
                                reason,
                                name,
                                bound,
                                no_infer: _,
                                id,
                            }) => Type::new(type_::TypeInner::GenericT(Box::new(GenericTData {
                                reason: reason.dupe(),
                                name: name.dupe(),
                                bound: bound.dupe(),
                                no_infer: true,
                                id: id.clone(),
                            }))),
                            _ => t,
                        }
                    } else {
                        t
                    }
                }
            };
            type_util::mod_reason_of_t(&|r| r.dupe().reposition(ref_loc.dupe()), &t)
        }
        Annot::NoInfer(t) => {
            let mut new_env = env.dupe();
            new_env.in_no_infer = true;
            merge_impl(&new_env, cx, file, t, false, false)
        }
        Annot::PropertyType(inner) => {
            let AnnotPropertyType { loc, obj, prop } = inner.as_ref();
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("$PropertyType"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let obj = merge_impl(env, cx, file, obj, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: obj,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::PropertyType {
                        name: Name::new(prop.dupe()),
                    }),
                )),
                id,
            })
        }
        Annot::ElementType(inner) => {
            let AnnotElementType { loc, obj, elem } = inner.as_ref();
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("$ElementType"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let obj = merge_impl(env, cx, file, obj, false, false);
            let index_type = merge_impl(env, cx, file, elem, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: obj,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::ElementType { index_type }),
                )),
                id,
            })
        }
        Annot::EnumValue(box (loc, t)) => {
            let reason = reason::mk_annot_reason(REnum { name: None }, loc.dupe());
            let representation_t = merge_impl(env, cx, file, t, false, false);
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::EnumValueT(Rc::new(type_::EnumInfo::new(
                    type_::EnumInfoInner::AbstractEnum { representation_t },
                )))),
            ))
        }
        Annot::Enum(box (loc, t)) => {
            let reason = reason::mk_annot_reason(REnum { name: None }, loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, cx, file, t, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::EnumType),
                )),
                id,
            })
        }
        Annot::OptionalIndexedAccessNonMaybeType(inner) => {
            let AnnotOptionalIndexedAccessNonMaybeType { loc, obj, index } = inner.as_ref();
            let reason = reason::mk_reason(RIndexedAccess { optional: true }, loc.dupe());
            let object_type = merge_impl(env, cx, file, obj, false, false);
            let index_type = merge_impl(env, cx, file, index, false, false);
            let object_reason = type_util::reason_of_t(&object_type).dupe();
            let index_reason = type_util::reason_of_t(&index_type).dupe();
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::IndexedTypeAccess {
                object: object_reason,
                index: index_reason,
            }));
            let id = eval_id_of_aloc(cx, loc.dupe());
            // let index = match index with ...
            let oia_index = match index {
                Pack::Packed::Annot(annot) => match annot.as_ref() {
                    Annot::SingletonString(box (_, str)) => {
                        type_::OptionalIndexedAccessIndex::OptionalIndexedAccessStrLitIndex(
                            Name::new(str.dupe()),
                        )
                    }
                    _ => type_::OptionalIndexedAccessIndex::OptionalIndexedAccessTypeIndex(
                        index_type,
                    ),
                },
                _ => type_::OptionalIndexedAccessIndex::OptionalIndexedAccessTypeIndex(index_type),
            };
            Type::new(type_::TypeInner::EvalT {
                type_: object_type,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::OptionalIndexedAccessNonMaybeType {
                        index: oia_index,
                    }),
                )),
                id,
            })
        }
        Annot::OptionalIndexedAccessResultType(inner) => {
            let AnnotOptionalIndexedAccessResultType {
                loc,
                non_maybe_result,
                void_loc,
            } = inner.as_ref();
            let reason = reason::mk_reason(RIndexedAccess { optional: true }, loc.dupe());
            let void_reason = reason::mk_reason(RVoid, void_loc.dupe());
            let non_maybe_result_type = merge_impl(env, cx, file, non_maybe_result, false, false);
            Type::new(type_::TypeInner::EvalT {
                type_: non_maybe_result_type,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    type_::unknown_use(),
                    reason,
                    Rc::new(type_::Destructor::OptionalIndexedAccessResultType { void_reason }),
                )),
                id: type_::eval::Id::generate_id(),
            })
        }
        Annot::NonMaybeType(box (loc, t)) => {
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("$NonMaybeType"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, cx, file, t, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::NonMaybeType),
                )),
                id,
            })
        }
        Annot::Omit(box (loc, t1, t2)) => {
            let reason = reason::mk_reason(RType(Name::new(FlowSmolStr::new("Omit"))), loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t1 = merge_impl(env, cx, file, t1, false, false);
            let t2 = merge_impl(env, cx, file, t2, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            let t2 = Type::new(type_::TypeInner::EvalT {
                type_: t2,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    type_::unknown_use(),
                    reason.dupe(),
                    Rc::new(type_::Destructor::MappedType {
                        homomorphic: type_::MappedTypeHomomorphicFlag::Unspecialized,
                        property_type: type_::mixed_t::make(reason.dupe()),
                        mapped_type_flags: type_::MappedTypeFlags {
                            optional: type_::MappedTypeOptionality::KeepOptionality,
                            variance: type_::MappedTypeVariance::KeepVariance,
                        },
                        distributive_tparam_name: None,
                    }),
                )),
                id: type_::eval::Id::generate_id(),
            });
            Type::new(type_::TypeInner::EvalT {
                type_: t1,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::RestType(
                        type_::object::rest::MergeMode::Omit,
                        t2,
                    )),
                )),
                id,
            })
        }
        Annot::ReadOnly(box (loc, t)) => {
            let reason = reason::mk_reason(RReadOnlyType, loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, cx, file, t, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::ReadOnlyType),
                )),
                id,
            })
        }
        Annot::Partial(box (loc, t)) => {
            let t = merge_impl(env, cx, file, t, false, false);
            let reason = reason::mk_reason(
                RPartialOf(Arc::new(type_util::desc_of_t(&t).clone())),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::PartialType),
                )),
                id,
            })
        }
        Annot::Required(box (loc, t)) => {
            let t = merge_impl(env, cx, file, t, false, false);
            let reason = reason::mk_reason(
                RRequiredOf(Arc::new(type_util::desc_of_t(&t).clone())),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::RequiredType),
                )),
                id,
            })
        }
        Annot::Keys(box (loc, t)) => {
            let reason = reason::mk_reason(RKeySet, loc.dupe());
            let t = merge_impl(env, cx, file, t, false, false);
            Type::new(type_::TypeInner::KeysT(reason, t))
        }
        Annot::Values(box (loc, t)) => {
            let reason =
                reason::mk_reason(RType(Name::new(FlowSmolStr::new("$Values"))), loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, cx, file, t, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::ValuesType),
                )),
                id,
            })
        }
        Annot::Exact(box (loc, t)) => {
            let t = merge_impl(env, cx, file, t, false, false);
            let desc = type_util::desc_of_t(&t).clone();
            let reason = reason::mk_annot_reason(RExactType(Arc::new(desc)), loc.dupe());
            let t = type_util::push_type_alias_reason(&reason, t);
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::ExactType),
                )),
                id,
            })
        }
        Annot::ExportsT(box (loc, mref)) => {
            let reason = reason::mk_annot_reason(
                reason::VirtualReasonDesc::RModule(mref.dupe()),
                loc.dupe(),
            );
            let symbol = Symbol::mk_module_symbol(mref.dupe().into_inner(), loc.dupe());
            let f: ResolvedRequire<'cx> = match cx.builtin_module_opt(mref) {
                Some((reason, lazy_module)) => {
                    ResolvedRequire::TypedModule(annotation_inference::force_module_type_thunk(
                        cx.dupe(),
                        type_::constraint::forcing_state::ModuleTypeForcingState::of_lazy_module(
                            reason,
                            lazy_module,
                        ),
                    ))
                }
                None => {
                    let err_t = flow_js_utils::lookup_builtin_module_error(
                        cx,
                        &mref.dupe().into_inner(),
                        loc.dupe(),
                    )
                    .unwrap();
                    ResolvedRequire::TypedModule(Rc::new(
                        move |_cx: &Context, _dst_cx: &Context| Err(err_t.dupe()),
                    ))
                }
            };
            annotation_inference::cjs_require(cx, reason, symbol, false, false, f)
        }
        Annot::Conditional(inner) => {
            let AnnotConditional {
                loc,
                distributive_tparam,
                infer_tparams,
                check_type,
                extends_type,
                true_type,
                false_type,
            } = inner.as_ref();
            let reason = reason::mk_reason(RConditionalType, loc.dupe());
            let id = eval_id_of_aloc(cx, loc.dupe());
            let convert = |distributive_tparam_name: Option<SubstName>, env: &MergeEnv| {
                let check_t = merge_impl(env, cx, file, check_type, false, false);
                let (tps_for_true_type, infer_tps_for_extends_types, infer_tparams) = {
                    let tparam_list: Vec<&TParam<ALoc, Pack::Packed<ALoc>>> = match infer_tparams {
                        TParams::Mono => vec![],
                        TParams::Poly(_, tps) => tps.iter().collect(),
                    };
                    let (new_tps, new_infer_tps, rev_tparams) = tparam_list.iter().fold(
                        (
                            FlowOrdMap::<FlowSmolStr, Type>::new(),
                            FlowOrdMap::<FlowSmolStr, (ALoc, Type)>::new(),
                            Vec::<type_::TypeParam>::new(),
                        ),
                        |(mut new_tps, mut new_infer_tps, mut rev_tparams), tp| {
                            let name_loc = &tp.name_loc;
                            let (tp_out, (name, _, t, _)) =
                                merge_tparam(&mut env.dupe(), cx, file, tp, true);
                            let name_str = name.string_of_subst_name().dupe();
                            new_tps.insert(name_str.dupe(), t.dupe());
                            // It's possible that for a given conditional extends type scope,
                            // both a regular generic type and infer type exists. e.g.
                            // type Both<T> = string extends [T, infer T] ? ... : ...
                            // To distinguish, we attach additional def_loc information for infer type.
                            new_infer_tps.insert(name_str, (name_loc.dupe(), t));
                            rev_tparams.push(tp_out);
                            (new_tps, new_infer_tps, rev_tparams)
                        },
                    );
                    let mut tps_for_true = env.tps.dupe();
                    for (k, v) in new_tps {
                        tps_for_true.insert(k, v);
                    }
                    let infer_tparams_out = rev_tparams;
                    (tps_for_true, new_infer_tps, infer_tparams_out)
                };
                let extends_env = MergeEnv {
                    infer_tps: infer_tps_for_extends_types,
                    ..env.dupe()
                };
                let extends_t = merge_impl(&extends_env, cx, file, extends_type, false, false);
                let true_env = MergeEnv {
                    tps: tps_for_true_type,
                    ..env.dupe()
                };
                let true_t = merge_impl(&true_env, cx, file, true_type, false, false);
                let false_t = merge_impl(env, cx, file, false_type, false, false);
                Type::new(type_::TypeInner::EvalT {
                    type_: check_t,
                    defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                        type_::unknown_use(),
                        reason.dupe(),
                        Rc::new(type_::Destructor::ConditionalType {
                            distributive_tparam_name,
                            infer_tparams: infer_tparams.into(),
                            extends_t,
                            true_t,
                            false_t,
                        }),
                    )),
                    id,
                })
            };
            match distributive_tparam {
                None => convert(None, env),
                Some(tp) => convert(Some(SubstName::name(tp.name.dupe())), env),
            }
        }
        Annot::ObjKeyMirror(inner) => {
            let AnnotObjKeyMirror { loc, obj } = inner.as_ref();
            let reason = reason::mk_reason(RObjectKeyMirror, loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let obj = merge_impl(env, cx, file, obj, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: obj,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::TypeMap(type_::TypeMap::ObjectKeyMirror)),
                )),
                id,
            })
        }
        Annot::ClassT(box (loc, t)) => {
            let t = merge_impl(env, cx, file, t, false, false);
            let desc = type_util::desc_of_t(&t).clone();
            let reason = reason::mk_reason(RStatics(Arc::new(desc)), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ClassT(t)),
            ))
        }
        Annot::FunctionBind(loc) => {
            let reason = reason::mk_annot_reason(RFunctionPrototype, loc.dupe());
            Type::new(type_::TypeInner::FunProtoBindT(reason))
        }
        Annot::ReactElementConfig(box (loc, t)) => {
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("React$ElementConfig"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, cx, file, t, false, false);
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    use_op,
                    reason,
                    Rc::new(type_::Destructor::ReactElementConfigType),
                )),
                id,
            })
        }
        Annot::Renders(inner) => {
            let AnnotRenders { loc, arg, variant } = inner.as_ref();
            let mut renders_env = env.dupe();
            renders_env.in_renders_arg = true;
            let t = merge_impl(&renders_env, cx, file, arg, false, false);
            let reason = reason::mk_annot_reason(
                RRenderType(Arc::new(type_util::reason_of_t(&t).desc(true).clone())),
                loc.dupe(),
            );
            match type_util::mk_possibly_generic_render_type(*variant, reason.dupe(), t.dupe()) {
                Some(t) => t,
                None => {
                    // let renders_variant = match variant with ...
                    let renders_variant = match variant {
                        flow_parser::ast::types::RendersVariant::Normal => {
                            type_::RendersVariant::RendersNormal
                        }
                        flow_parser::ast::types::RendersVariant::Maybe => {
                            type_::RendersVariant::RendersMaybe
                        }
                        flow_parser::ast::types::RendersVariant::Star => {
                            type_::RendersVariant::RendersStar
                        }
                    };
                    annotation_inference::mk_non_generic_render_type(cx, reason, renders_variant, t)
                }
            }
        }
        Annot::ComponentMissingRenders(loc) => {
            let reason = reason::mk_annot_reason(
                RRenderType(Arc::new(RType(Name::new(FlowSmolStr::new("React.Node"))))),
                loc.dupe(),
            );
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::RendersT(Rc::new(
                    type_::CanonicalRendersForm::DefaultRenders,
                ))),
            ))
        }
        Annot::FunAnnot(box (loc, def)) => {
            let reason = reason::mk_annot_reason(RFunctionType, loc.dupe());
            let statics = merge_fun_statics(env, cx, file, reason.dupe(), &BTreeMap::new());
            merge_fun(env, cx, file, reason, def, statics, false, false)
        }
        Annot::ComponentAnnot(box (loc, def)) => {
            let reason = reason::mk_annot_reason(RComponentType, loc.dupe());
            merge_component(env, cx, file, reason, true, def, None)
        }
        Annot::ObjAnnot(inner) => {
            let AnnotObjAnnot {
                loc,
                obj_kind,
                props,
                computed_props,
                proto,
            } = inner.as_ref();
            let reason = reason::mk_annot_reason(RObjectType, loc.dupe());
            let obj_kind = match obj_kind {
                type_sig::ObjKind::ExactObj => type_::ObjKind::Exact,
                type_sig::ObjKind::InexactObj => type_::ObjKind::Inexact,
                type_sig::ObjKind::IndexedObj(dict) => {
                    type_::ObjKind::Indexed(merge_dict(env, cx, file, dict, false))
                }
            };
            let mut props_smap: BTreeMap<FlowSmolStr, type_::Property> = props
                .iter()
                .map(|(key, prop)| {
                    let p = merge_obj_annot_prop(env, cx, file, prop);
                    (key.dupe(), p)
                })
                .collect();
            for (key, prop) in computed_props {
                if let Some(name) = resolve_computed_key(env, cx, file, key) {
                    let name_str = name.into_smol_str();
                    let t = merge_obj_annot_prop(env, cx, file, prop);
                    match props_smap.entry(name_str) {
                        std::collections::btree_map::Entry::Vacant(e) => {
                            e.insert(t);
                        }
                        std::collections::btree_map::Entry::Occupied(mut e) => {
                            let merged = merge_overloaded_property(e.get(), t);
                            e.insert(merged);
                        }
                    }
                }
            }
            let props_map: type_::properties::PropertiesMap = props_smap
                .into_iter()
                .map(|(k, v)| (Name::new(k), v))
                .collect();
            let mk_object = |call: Option<Type>, proto: Type| {
                let id = type_::properties::Id::of_aloc_id(true, cx.make_aloc_id(loc));
                let flags = type_::Flags {
                    obj_kind: obj_kind.clone(),
                    react_dro: None,
                };
                let call = call.map(|t| cx.make_call_prop(t));
                cx.add_property_map(id.dupe(), props_map.dupe());
                let obj_type = type_::mk_objecttype(Some(flags), None, call, id, proto);
                if obj_kind == type_::ObjKind::Exact {
                    let reason_op =
                        reason::mk_annot_reason(RExactType(Arc::new(RObjectType)), loc.dupe());
                    type_util::make_exact_object(reason.dupe(), Rc::new(obj_type), &reason_op)
                } else {
                    Type::new(type_::TypeInner::DefT(
                        reason.dupe(),
                        type_::DefT::new(type_::DefTInner::ObjT(Rc::new(obj_type))),
                    ))
                }
            };
            match proto {
                ObjAnnotProto::ObjAnnotImplicitProto => {
                    mk_object(None, Type::new(type_::TypeInner::ObjProtoT(reason.dupe())))
                }
                ObjAnnotProto::ObjAnnotExplicitProto(proto_loc, t) => {
                    let proto_reason = reason::mk_reason(RPrototype, proto_loc.dupe());
                    let proto = annotation_inference::obj_test_proto(
                        cx,
                        proto_reason.dupe(),
                        merge_impl(env, cx, file, t, false, false),
                    );
                    let proto = type_util::typeof_annotation(proto_reason, proto, None);
                    mk_object(None, proto)
                }
                ObjAnnotProto::ObjAnnotCallable { ts } => {
                    let proto = Type::new(type_::TypeInner::FunProtoT(reason.dupe()));
                    let ts_vec: Vec<Type> = ts
                        .iter()
                        .map(|t| {
                            let t = merge_impl(env, cx, file, t, false, false);
                            mk_object(Some(t), proto.dupe())
                        })
                        .collect();
                    match ts_vec.len() {
                        1 => ts_vec.into_iter().next().unwrap(),
                        _ => {
                            let mut iter = ts_vec.into_iter();
                            let t0 = iter.next().unwrap();
                            let t1 = iter.next().unwrap();
                            let rest: Vec<Type> = iter.collect();
                            let callable_reason =
                                reason::mk_annot_reason(RCallableObjectType, loc.dupe());
                            Type::new(type_::TypeInner::IntersectionT(
                                callable_reason,
                                type_::inter_rep::make(t0, t1, rest.into()),
                            ))
                        }
                    }
                }
            }
        }
        Annot::ObjSpreadAnnot(inner) => {
            let AnnotObjSpreadAnnot { loc, exact, elems } = inner.as_ref();
            let reason = reason::mk_annot_reason(RObjectType, loc.dupe());
            let target = type_::object::spread::Target::Annot { make_exact: *exact };
            let merge_slice = |dict: &Option<ObjAnnotDict<Pack::Packed<ALoc>>>,
                               props: &BTreeMap<
                FlowSmolStr,
                ObjAnnotProp<ALoc, Pack::Packed<ALoc>>,
            >,
                               computed_props: &[(
                Pack::Packed<ALoc>,
                ObjAnnotProp<ALoc, Pack::Packed<ALoc>>,
            )]| {
                let dict = dict.as_ref().map(|d| merge_dict(env, cx, file, d, false));
                let mut props_smap: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
                for (key, prop) in props {
                    let p = merge_obj_annot_prop(env, cx, file, prop);
                    props_smap.insert(key.dupe(), p);
                }
                for (key, prop) in computed_props {
                    if let Some(name) = resolve_computed_key(env, cx, file, key) {
                        let name_str = name.into_smol_str();
                        let t = merge_obj_annot_prop(env, cx, file, prop);
                        match props_smap.entry(name_str) {
                            std::collections::btree_map::Entry::Vacant(e) => {
                                e.insert(t);
                            }
                            std::collections::btree_map::Entry::Occupied(mut e) => {
                                let merged = merge_overloaded_property(e.get(), t);
                                e.insert(merged);
                            }
                        }
                    }
                }
                let prop_map = props_smap
                    .into_iter()
                    .map(|(k, v)| (Name::new(k), v))
                    .collect();
                type_::object::spread::OperandSlice::new(type_::object::spread::OperandSliceInner {
                    reason: reason.dupe(),
                    prop_map,
                    generics: flow_typing_generics::spread_empty(),
                    dict,
                    reachable_targs: vec![].into(),
                })
            };
            let merge_elem = |elem: &ObjSpreadAnnotElem<ALoc, Pack::Packed<ALoc>>| match elem {
                ObjSpreadAnnotElem::ObjSpreadAnnotElem(t) => {
                    type_::object::spread::Operand::Type(merge_impl(env, cx, file, t, false, false))
                }
                ObjSpreadAnnotElem::ObjSpreadAnnotSlice {
                    dict,
                    props,
                    computed_props,
                } => {
                    type_::object::spread::Operand::Slice(merge_slice(dict, props, computed_props))
                }
            };
            let mut merged_elems_rev: Vec<type_::object::spread::Operand> =
                elems.iter().rev().map(merge_elem).collect();
            let first = merged_elems_rev.remove(0);
            let (t, todo_rev, head_slice) = match first {
                type_::object::spread::Operand::Type(t) => (t, merged_elems_rev, None),
                type_::object::spread::Operand::Slice(slice) => match merged_elems_rev.first() {
                    Some(type_::object::spread::Operand::Type(_)) => {
                        let second = merged_elems_rev.remove(0);
                        match second {
                            type_::object::spread::Operand::Type(t) => {
                                (t, merged_elems_rev, Some(slice))
                            }
                            _ => unreachable!(),
                        }
                    }
                    Some(type_::object::spread::Operand::Slice(_)) => {
                        panic!("unexpected adjacent slices")
                    }
                    None => {
                        panic!("unexpected solo slice")
                    }
                },
            };
            let id = eval_id_of_aloc(cx, loc.dupe());
            Type::new(type_::TypeInner::EvalT {
                type_: t,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    type_::unknown_use(),
                    reason,
                    Rc::new(type_::Destructor::SpreadType(
                        target,
                        todo_rev.into(),
                        head_slice,
                    )),
                )),
                id,
            })
        }
        Annot::InlineInterface(box (loc, def)) => {
            let reason = reason::mk_annot_reason(RInterfaceType, loc.dupe());
            let id = cx.make_aloc_id(loc);
            merge_interface(env, cx, file, reason, None, id, def, true, vec![])
        }
        Annot::MappedTypeAnnot(inner) => {
            let AnnotMappedTypeAnnot {
                loc,
                source_type,
                property_type,
                key_tparam,
                variance,
                variance_op,
                optional,
                inline_keyof,
            } = inner.as_ref();
            let source_type = merge_impl(env, cx, file, source_type, false, false);
            let mut env = env.dupe();
            let (tp, _) = merge_tparam(&mut env, cx, file, key_tparam, false);
            let property_type = {
                let prop_type = merge_impl(&env, cx, file, property_type, false, false);
                let prop_reason = type_util::reason_of_t(&prop_type).dupe();
                let id = cx.make_source_poly_id(true, loc);
                Type::new(type_::TypeInner::DefT(
                    prop_reason,
                    type_::DefT::new(type_::DefTInner::PolyT(Box::new(type_::PolyTData {
                        tparams_loc: loc.dupe(),
                        tparams: vec![tp].into(),
                        t_out: prop_type,
                        id,
                    }))),
                ))
            };
            let optional = {
                use flow_parser::ast::types::object::MappedTypeOptionalFlag;
                match optional {
                    MappedTypeOptionalFlag::PlusOptional | MappedTypeOptionalFlag::Optional => {
                        type_::MappedTypeOptionality::MakeOptional
                    }
                    MappedTypeOptionalFlag::MinusOptional => {
                        type_::MappedTypeOptionality::RemoveOptional
                    }
                    MappedTypeOptionalFlag::NoOptionalFlag => {
                        type_::MappedTypeOptionality::KeepOptionality
                    }
                }
            };
            let mapped_type_variance = match variance_op {
                Some(flow_parser::ast::types::object::MappedTypeVarianceOp::Add) => {
                    type_::MappedTypeVariance::OverrideVariance(*variance)
                }
                Some(flow_parser::ast::types::object::MappedTypeVarianceOp::Remove) => {
                    type_::MappedTypeVariance::RemoveVariance(*variance)
                }
                None => {
                    if *variance == Polarity::Neutral {
                        type_::MappedTypeVariance::KeepVariance
                    } else {
                        type_::MappedTypeVariance::OverrideVariance(*variance)
                    }
                }
            };
            let mapped_type_flags = type_::MappedTypeFlags {
                variance: mapped_type_variance,
                optional,
            };
            let id = eval_id_of_aloc(cx, loc.dupe());
            let reason = reason::mk_reason(RObjectType, loc.dupe());
            let (source_type, homomorphic) = if *inline_keyof {
                (source_type, type_::MappedTypeHomomorphicFlag::Homomorphic)
            } else {
                match &*source_type {
                    type_::TypeInner::GenericT(box GenericTData { bound, .. }) => match &**bound {
                        type_::TypeInner::KeysT(_, obj_t) => (
                            obj_t.dupe(),
                            type_::MappedTypeHomomorphicFlag::SemiHomomorphic(source_type.dupe()),
                        ),
                        _ => (source_type, type_::MappedTypeHomomorphicFlag::Unspecialized),
                    },
                    type_::TypeInner::OpenT(tvar) => {
                        let (_, constraints) = cx.find_constraints(tvar.id() as i32);
                        match constraints {
                            type_::constraint::Constraints::FullyResolved(s) => {
                                let forced = cx.force_fully_resolved_tvar(&s);
                                match &*forced {
                                    type_::TypeInner::GenericT(box GenericTData {
                                        bound, ..
                                    }) => match &**bound {
                                        type_::TypeInner::KeysT(_, obj_t) => (
                                            obj_t.dupe(),
                                            type_::MappedTypeHomomorphicFlag::SemiHomomorphic(
                                                source_type.dupe(),
                                            ),
                                        ),
                                        _ => (
                                            source_type,
                                            type_::MappedTypeHomomorphicFlag::Unspecialized,
                                        ),
                                    },
                                    _ => (
                                        source_type,
                                        type_::MappedTypeHomomorphicFlag::Unspecialized,
                                    ),
                                }
                            }
                            _ => (source_type, type_::MappedTypeHomomorphicFlag::Unspecialized),
                        }
                    }
                    _ => (source_type, type_::MappedTypeHomomorphicFlag::Unspecialized),
                }
            };
            let distributive_tparam_name: Option<SubstName> = match &*source_type {
                type_::TypeInner::GenericT(box GenericTData { name, .. }) => Some(name.dupe()),
                type_::TypeInner::OpenT(tvar) => {
                    let (_, constraints) = cx.find_constraints(tvar.id() as i32);
                    match constraints {
                        type_::constraint::Constraints::FullyResolved(s) => {
                            let forced = cx.force_fully_resolved_tvar(&s);
                            match &*forced {
                                type_::TypeInner::GenericT(box GenericTData { name, .. }) => {
                                    Some(name.dupe())
                                }
                                _ => None,
                            }
                        }
                        _ => None,
                    }
                }
                _ => None,
            };
            Type::new(type_::TypeInner::EvalT {
                type_: source_type,
                defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                    type_::unknown_use(),
                    reason,
                    Rc::new(type_::Destructor::MappedType {
                        property_type,
                        mapped_type_flags,
                        homomorphic,
                        distributive_tparam_name,
                    }),
                )),
                id,
            })
        }
    }
}

fn merge_value<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    value: &Pack::PackedValue<ALoc>,
    as_const: bool,
    const_decl: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_type_sig::type_sig::Value;
    match value {
        Value::ClassExpr(box (loc, def)) => {
            let name: FlowSmolStr = "<<anonymous class>>".into();
            let reason = type_::desc_format::instance_reason(Name::new(name), loc.dupe());
            let id = cx.make_aloc_id(loc);
            merge_class(env, cx, file, reason, None, id, def)
        }
        Value::FunExpr(inner) => {
            let ValueFunExpr {
                loc,
                async_,
                generator,
                def,
                statics,
            } = inner.as_ref();
            let reason = reason::func_reason(*async_, *generator, loc.dupe());
            let statics_t = merge_fun_statics(env, cx, file, reason.dupe(), statics);
            merge_fun(env, cx, file, reason, def, statics_t, false, false)
        }
        Value::StringVal(loc) => {
            let reason = reason::mk_reason(RString, loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::StrGeneralT(type_::Literal::AnyLiteral)),
            ))
        }
        Value::StringLit(box (loc, lit)) => {
            if as_const || const_decl {
                let reason = reason::mk_annot_reason(RStringLit(Name::new(lit.dupe())), loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::SingletonStrT {
                        from_annot: as_const,
                        value: Name::new(lit.dupe()),
                    }),
                ))
            } else {
                let reason = reason::mk_reason(RString, loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::StrGeneralT(type_::Literal::AnyLiteral)),
                ))
            }
        }
        Value::NumberVal(loc) => {
            let reason = reason::mk_reason(RNumber, loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::NumGeneralT(type_::Literal::AnyLiteral)),
            ))
        }
        Value::NumberLit(box (loc, num, raw)) => {
            if as_const || const_decl {
                let reason = reason::mk_annot_reason(RNumberLit(raw.dupe()), loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::SingletonNumT {
                        from_annot: as_const,
                        value: type_::NumberLiteral(*num, raw.dupe()),
                    }),
                ))
            } else {
                let reason = reason::mk_reason(RNumber, loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::NumGeneralT(type_::Literal::AnyLiteral)),
                ))
            }
        }
        Value::BigIntVal(loc) => {
            let reason = reason::mk_reason(RBigInt, loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::BigIntGeneralT(type_::Literal::AnyLiteral)),
            ))
        }
        Value::BigIntLit(box (loc, bigint, raw)) => {
            if as_const || const_decl {
                let reason = reason::mk_annot_reason(RBigIntLit(raw.dupe()), loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::SingletonBigIntT {
                        from_annot: as_const,
                        value: type_::BigIntLiteral(*bigint, raw.dupe()),
                    }),
                ))
            } else {
                let reason = reason::mk_reason(RBigInt, loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::BigIntGeneralT(type_::Literal::AnyLiteral)),
                ))
            }
        }
        Value::BooleanVal(loc) => {
            let reason = reason::mk_reason(RBoolean, loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::BoolGeneralT),
            ))
        }
        Value::BooleanLit(loc, lit) => {
            if as_const || const_decl {
                let reason = reason::mk_annot_reason(RBooleanLit(*lit), loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::SingletonBoolT {
                        from_annot: as_const,
                        value: *lit,
                    }),
                ))
            } else {
                let reason = reason::mk_reason(RBoolean, loc.dupe());
                Type::new(type_::TypeInner::DefT(
                    reason,
                    type_::DefT::new(type_::DefTInner::BoolGeneralT),
                ))
            }
        }
        Value::NullLit(loc) => type_::null::at(loc.dupe()),
        Value::DeclareModuleImplicitlyExportedObject(inner) => {
            merge_declare_module_implicitly_exported_object(
                env,
                cx,
                file,
                inner.loc.dupe(),
                &inner.module_name,
                &inner.props,
            )
        }
        Value::ObjLit(inner) => merge_object_lit(
            env,
            cx,
            file,
            inner.loc.dupe(),
            inner.frozen,
            &inner.proto,
            &inner.props,
            false,
            as_const,
        ),
        Value::ObjSpreadLit(inner) => merge_obj_spread_lit(
            env,
            cx,
            file,
            inner.loc.dupe(),
            inner.frozen,
            &inner.proto,
            &inner.elems,
            false,
            as_const,
        ),
        Value::EmptyConstArrayLit(loc) => {
            let reason = reason::mk_reason(RConstArrayLit, loc.dupe());
            let elem_t = type_::empty_t::make(reason::mk_reason(REmptyArrayElement, loc.dupe()));
            let arrtype = type_::ArrType::TupleAT {
                elem_t,
                elements: vec![].into(),
                react_dro: None,
                arity: (0, 0),
                inexact: false,
            };
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(arrtype))),
            ))
        }
        Value::ArrayLit(box (loc, t, ts)) => {
            let reason = if as_const {
                reason::mk_reason(RConstArrayLit, loc.dupe())
            } else {
                reason::mk_reason(RArrayLit, loc.dupe())
            };
            let t = merge_impl(env, cx, file, t, as_const, false);
            let ts: Vec<Type> = ts
                .iter()
                .map(|t| merge_impl(env, cx, file, t, as_const, false))
                .collect();
            let elem_t = if ts.is_empty() {
                t.dupe()
            } else {
                let t0 = t.dupe();
                let t1 = ts[0].dupe();
                let rest: Vec<Type> = ts[1..].to_vec();
                let source_aloc = Some(cx.make_aloc_id(loc));
                Type::new(type_::TypeInner::UnionT(
                    reason.dupe(),
                    type_::union_rep::make(
                        source_aloc,
                        type_::union_rep::UnionKind::UnknownKind,
                        t0,
                        t1,
                        rest.into(),
                    ),
                ))
            };
            let arrtype = if as_const {
                let mut all_ts = vec![t.dupe()];
                all_ts.extend(ts.iter().duped());
                let elements: Vec<type_::TupleElement> = all_ts
                    .iter()
                    .map(|t| {
                        let reason = reason::mk_reason(RArrayLit, type_util::loc_of_t(t).dupe());
                        type_util::mk_tuple_element(
                            reason,
                            t.dupe(),
                            None,
                            false,
                            Polarity::Positive,
                        )
                    })
                    .collect();
                let num_elts = elements.len() as i32;
                type_::ArrType::TupleAT {
                    elem_t,
                    elements: elements.into(),
                    react_dro: None,
                    arity: (num_elts, num_elts),
                    inexact: false,
                }
            } else {
                type_::ArrType::ArrayAT {
                    elem_t,
                    tuple_view: None,
                    react_dro: None,
                }
            };
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(arrtype))),
            ))
        }
        Value::AsConst(value) => merge_value(env, cx, file, value, true, false),
    }
}

fn merge_declare_module_implicitly_exported_object<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    loc: ALoc,
    module_name: &flow_common::flow_import_specifier::Userland,
    props: &BTreeMap<FlowSmolStr, ObjValueProp<ALoc, Pack::Packed<ALoc>>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let reason = reason::mk_reason(RModule(module_name.dupe()), loc);
    let proto = Type::new(type_::TypeInner::ObjProtoT(reason.dupe()));
    let props_map: type_::properties::PropertiesMap = props
        .iter()
        .map(|(key, prop)| {
            let p = merge_obj_value_prop(env, cx, file, key, prop, true, false, false);
            (Name::new(key.dupe()), p)
        })
        .collect();
    obj_type::mk_with_proto(
        cx,
        reason,
        type_::ObjKind::Exact,
        None,
        None,
        Some(props_map),
        None,
        proto,
    )
}

fn merge_object_lit<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    loc: ALoc,
    frozen: bool,
    proto: &Option<(ALoc, Pack::Packed<ALoc>)>,
    props: &BTreeMap<FlowSmolStr, ObjValueProp<ALoc, Pack::Packed<ALoc>>>,
    for_export: bool,
    as_const: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let reason = reason::mk_obj_lit_reason(as_const, frozen, || false, loc.dupe());
    let proto_t = match proto {
        None => Type::new(type_::TypeInner::ObjProtoT(reason.dupe())),
        Some((proto_loc, t)) => {
            let proto_reason = reason::mk_reason(RPrototype, proto_loc.dupe());
            let proto = annotation_inference::obj_test_proto(
                cx,
                proto_reason.dupe(),
                merge_impl(env, cx, file, t, false, false),
            );
            type_util::typeof_annotation(proto_reason, proto, None)
        }
    };
    let props_map: type_::properties::PropertiesMap = props
        .iter()
        .map(|(key, prop)| {
            let p = merge_obj_value_prop(env, cx, file, key, prop, for_export, as_const, frozen);
            (Name::new(key.dupe()), p)
        })
        .collect();
    obj_type::mk_with_proto(
        cx,
        reason,
        type_::ObjKind::Exact,
        None,
        None,
        Some(props_map),
        None,
        proto_t,
    )
}

fn merge_obj_spread_lit<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    loc: ALoc,
    frozen: bool,
    _proto: &Option<(ALoc, Pack::Packed<ALoc>)>,
    elems: &Vec1<ObjValueSpreadElem<ALoc, Pack::Packed<ALoc>>>,
    for_export: bool,
    as_const: bool,
) -> Type {
    let reason = reason::mk_obj_lit_reason(as_const, frozen, || false, loc.dupe());
    // TODO: fix spread to use provided __proto__ prop
    let merge_slice = |props: &BTreeMap<FlowSmolStr, ObjValueProp<ALoc, Pack::Packed<ALoc>>>| {
        let mut prop_map = BTreeMap::new();
        for (key, prop) in props {
            let p = merge_obj_value_prop(env, cx, file, key, prop, for_export, as_const, frozen);
            prop_map.insert(Name::new(key.dupe()), p);
        }
        type_::object::spread::OperandSlice::new(type_::object::spread::OperandSliceInner {
            reason: reason.dupe(),
            prop_map: prop_map.into_iter().collect(),
            generics: flow_typing_generics::spread_empty(),
            dict: None,
            reachable_targs: vec![].into(),
        })
    };
    let merge_elem = |elem: &ObjValueSpreadElem<ALoc, Pack::Packed<ALoc>>| match elem {
        ObjValueSpreadElem::ObjValueSpreadElem(t) => {
            type_::object::spread::Operand::Type(merge_impl(env, cx, file, t, as_const, false))
        }
        ObjValueSpreadElem::ObjValueSpreadSlice(props) => {
            type_::object::spread::Operand::Slice(merge_slice(props))
        }
    };
    let mut merged_elems_rev: Vec<type_::object::spread::Operand> =
        elems.iter().rev().map(merge_elem).collect();
    let first = merged_elems_rev.remove(0);
    let (t, todo_rev, head_slice) = match first {
        type_::object::spread::Operand::Type(t) => (t, merged_elems_rev, None),
        type_::object::spread::Operand::Slice(slice) => match merged_elems_rev.first() {
            Some(type_::object::spread::Operand::Type(_)) => {
                let second = merged_elems_rev.remove(0);
                match second {
                    type_::object::spread::Operand::Type(t) => (t, merged_elems_rev, Some(slice)),
                    _ => unreachable!(),
                }
            }
            _ => panic!("unexpected spread"),
        },
    };
    let make_seal = if frozen {
        type_::object::spread::SealType::Frozen
    } else if as_const {
        type_::object::spread::SealType::AsConst
    } else {
        type_::object::spread::SealType::Sealed
    };
    let target = type_::object::spread::Target::Value { make_seal };
    let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::ObjectSpread {
        op: reason.dupe(),
    }));
    let acc = match &head_slice {
        Some(slice) => {
            vec![flow_typing_type::type_::object::spread::AccElement::InlineSlice(slice.clone())]
        }
        None => vec![],
    };
    let state = type_::object::spread::State {
        todo_rev: todo_rev.into(),
        acc: acc.into(),
        spread_id: reason::mk_id() as i32,
        union_reason: None,
        curr_resolve_idx: 0,
    };
    annotation_inference::object_spread(cx, use_op, reason, target, state, t)
}

fn merge_accessor<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    accessor: &Accessor<ALoc, Pack::Packed<ALoc>>,
) -> type_::Property {
    match accessor {
        Accessor::Get(loc, t) => {
            let type_ = merge_impl(env, cx, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Get {
                key_loc: Some(loc.dupe()),
                type_,
            })
        }
        Accessor::Set(loc, t) => {
            let type_ = merge_impl(env, cx, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Set {
                key_loc: Some(loc.dupe()),
                type_,
            })
        }
        Accessor::GetSet(gloc, gt, sloc, st) => {
            let get_type = merge_impl(env, cx, file, gt, false, false);
            let set_type = merge_impl(env, cx, file, st, false, false);
            type_::Property::new(type_::PropertyInner::GetSet(Box::new(type_::GetSetData {
                get_key_loc: Some(gloc.dupe()),
                get_type,
                set_key_loc: Some(sloc.dupe()),
                set_type,
            })))
        }
    }
}

fn merge_obj_value_prop<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    key: &FlowSmolStr,
    prop: &ObjValueProp<ALoc, Pack::Packed<ALoc>>,
    for_export: bool,
    as_const: bool,
    frozen: bool,
) -> type_::Property {
    match prop {
        ObjValueProp::ObjValueField(id_loc, Pack::Packed::Ref(ref_), polarity) if for_export => {
            let polarity = if as_const || frozen {
                Polarity::Positive
            } else {
                *polarity
            };
            let id_loc = id_loc.dupe();
            let key = key.dupe();
            merge_ref(
                cx,
                file,
                move |type_, ref_loc, def_loc, value_name| {
                    // If name matches and prop loc is the name as ref loc, it must be shorthand syntax like
                    // module.exports = { foo }.
                    if key == *value_name && id_loc == ref_loc {
                        type_::Property::new(type_::PropertyInner::Field(Box::new(
                            type_::FieldData {
                                preferred_def_locs: Some(Vec1::new(def_loc)),
                                key_loc: Some(id_loc),
                                type_,
                                polarity,
                            },
                        )))
                    } else {
                        type_::Property::new(type_::PropertyInner::Field(Box::new(
                            type_::FieldData {
                                preferred_def_locs: None,
                                key_loc: Some(id_loc),
                                type_,
                                polarity,
                            },
                        )))
                    }
                },
                ref_,
                false,
            )
        }
        ObjValueProp::ObjValueField(id_loc, t, polarity) => {
            let type_ = merge_impl(env, cx, file, t, as_const, false);
            let polarity = if as_const || frozen {
                Polarity::Positive
            } else {
                *polarity
            };
            type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_,
                polarity,
            })))
        }
        ObjValueProp::ObjValueAccess(x) => merge_accessor(env, cx, file, x),
        ObjValueProp::ObjValueMethod {
            id_loc,
            fn_loc,
            async_,
            generator,
            def,
        } => {
            let reason = reason::func_reason(*async_, *generator, fn_loc.dupe());
            let statics = merge_fun_statics(env, cx, file, reason.dupe(), &BTreeMap::new());
            let type_ = merge_fun(env, cx, file, reason, def, statics, false, false);
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_class_prop<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    prop: &ObjValueProp<ALoc, Pack::Packed<ALoc>>,
) -> type_::Property {
    match prop {
        ObjValueProp::ObjValueField(id_loc, t, polarity) => {
            let type_ = merge_impl(env, cx, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_,
                polarity: *polarity,
            })))
        }
        ObjValueProp::ObjValueAccess(x) => merge_accessor(env, cx, file, x),
        ObjValueProp::ObjValueMethod {
            id_loc,
            fn_loc,
            async_,
            generator,
            def,
        } => {
            let reason = reason::func_reason(*async_, *generator, fn_loc.dupe());
            let statics = type_::dummy_static(reason.dupe());
            let type_ = merge_fun(env, cx, file, reason, def, statics, true, false);
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_obj_annot_prop<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    prop: &ObjAnnotProp<ALoc, Pack::Packed<ALoc>>,
) -> type_::Property {
    use flow_common::reason::VirtualReasonDesc::*;
    match prop {
        ObjAnnotProp::ObjAnnotField(id_loc, t, polarity) => {
            let type_ = merge_impl(env, cx, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_,
                polarity: *polarity,
            })))
        }
        ObjAnnotProp::ObjAnnotAccess(x) => merge_accessor(env, cx, file, x),
        ObjAnnotProp::ObjAnnotMethod {
            id_loc,
            fn_loc,
            def,
        } => {
            let reason = reason::mk_annot_reason(RFunctionType, fn_loc.dupe());
            let statics = merge_fun_statics(env, cx, file, reason.dupe(), &BTreeMap::new());
            let type_ = merge_fun(env, cx, file, reason, def, statics, false, false);
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_interface_prop<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    prop: &InterfaceProp<ALoc, Pack::Packed<ALoc>>,
    is_static: bool,
) -> type_::Property {
    use flow_common::reason::VirtualReasonDesc::*;
    match prop {
        InterfaceProp::InterfaceField(id_loc, t, polarity) => {
            let t = merge_impl(env, cx, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
                preferred_def_locs: None,
                key_loc: id_loc.dupe(),
                type_: t,
                polarity: *polarity,
            })))
        }
        InterfaceProp::InterfaceAccess(x) => merge_accessor(env, cx, file, x),
        InterfaceProp::InterfaceMethod(ms) => {
            let merge_method = |fn_loc: &ALoc, def: &FunSig<ALoc, Pack::Packed<ALoc>>| {
                let reason = reason::mk_reason(RFunctionType, fn_loc.dupe());
                let statics = type_::dummy_static(reason.dupe());
                merge_fun(env, cx, file, reason, def, statics, true, is_static)
            };
            let (ref first_id_loc, ref first_fn_loc, ref first_def) = ms[0];
            let mut all_types: Vec<Type> = vec![merge_method(first_fn_loc, first_def)];
            for (_id_loc, fn_loc, def) in &ms[1..] {
                all_types.push(merge_method(fn_loc, def));
            }
            let type_ = if all_types.len() == 1 {
                all_types.pop().unwrap()
            } else {
                let mut iter = all_types.into_iter();
                let t0 = iter.next().unwrap();
                let t1 = iter.next().unwrap();
                let reason = type_util::reason_of_t(&t0).dupe();
                Type::new(type_::TypeInner::IntersectionT(
                    reason,
                    type_::inter_rep::make(t0, t1, iter.collect::<Vec<_>>().into()),
                ))
            };
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(first_id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_dict<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    dict: &ObjAnnotDict<Pack::Packed<ALoc>>,
    as_const: bool,
) -> type_::DictType {
    let key = merge_impl(env, cx, file, &dict.key, false, false);
    let value = merge_impl(env, cx, file, &dict.value, false, false);
    let polarity = Polarity::apply_const(as_const, dict.polarity);
    type_::DictType {
        dict_name: dict.name.dupe(),
        dict_polarity: polarity,
        key,
        value,
    }
}

fn merge_tparams_targs<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    t: impl FnOnce(&Context<'cx>, &MergeEnv, Vec<(SubstName, Reason, Type, Polarity)>) -> Type,
    tparams: &TParams<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    match tparams {
        TParams::Mono => t(cx, env, Vec::new()),
        TParams::Poly(tparams_loc, tps) => {
            let poly_reason = reason.update_desc(|d| RPolyType(Arc::new(d)));
            let mut current_env = env.dupe();
            let mut tparams_vec: Vec<type_::TypeParam> = Vec::new();
            let mut tparam_tuples: Vec<(SubstName, Reason, Type, Polarity)> = Vec::new();
            for tp in tps.iter() {
                let (tp, tuple) = merge_tparam(&mut current_env, cx, file, tp, false);
                tparams_vec.push(tp);
                tparam_tuples.push(tuple);
            }
            let t_out = t(cx, &current_env, tparam_tuples);
            let id = cx.make_source_poly_id(true, tparams_loc);
            Type::new(type_::TypeInner::DefT(
                poly_reason,
                type_::DefT::new(type_::DefTInner::PolyT(Box::new(type_::PolyTData {
                    tparams_loc: tparams_loc.dupe(),
                    tparams: tparams_vec.into(),
                    t_out,
                    id,
                }))),
            ))
        }
    }
}

fn merge_tparam<'cx>(
    env: &mut MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    tp: &TParam<ALoc, Pack::Packed<ALoc>>,
    from_infer: bool,
) -> (type_::TypeParam, (SubstName, Reason, Type, Polarity)) {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_common::subst_name::SubstName;

    let TParam {
        name_loc,
        name,
        polarity,
        bound,
        default,
        is_const,
    } = tp;
    let reason = reason::mk_reason(RType(Name::new(name.dupe())), name_loc.dupe());
    let bound = match bound {
        None => {
            let bound_reason = reason.dupe().replace_desc(RMixed);
            Type::new(type_::TypeInner::DefT(
                bound_reason,
                type_::DefT::new(type_::DefTInner::MixedT(
                    type_::MixedFlavor::MixedEverything,
                )),
            ))
        }
        Some(t) => merge_impl(env, cx, file, t, false, false),
    };
    let default = default
        .as_ref()
        .map(|t| merge_impl(env, cx, file, t, false, false));
    let subst_name = if from_infer && env.tps.contains_key(name) {
        let fvs: flow_data_structure_wrapper::ord_set::FlowOrdSet<SubstName> =
            env.tps.keys().map(|n| SubstName::name(n.dupe())).collect();
        flow_typing_flow_common::type_subst::new_name(&SubstName::name(name.dupe()), &fvs)
    } else {
        SubstName::name(name.dupe())
    };
    let tp = type_::TypeParam::new(type_::TypeParamInner {
        reason: reason.dupe(),
        name: subst_name,
        bound,
        polarity: *polarity,
        default,
        is_this: false,
        is_const: *is_const,
    });
    let t = flow_js_utils::generic_of_tparam(cx, |x: &Type| x.dupe(), &tp);
    env.tps.insert(name.dupe(), t.dupe());
    (tp, (SubstName::name(name.dupe()), reason, t, *polarity))
}

fn merge_op<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    op: &Op<Box<Pack::Packed<ALoc>>>,
) -> Op<Type> {
    op.map(&mut (), |_, t| merge_impl(env, cx, file, t, false, false))
}

fn merge_interface<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    class_name: Option<FlowSmolStr>,
    id: flow_aloc::ALocId,
    def: &InterfaceSig<ALoc, Pack::Packed<ALoc>>,
    inline: bool,
    targs: Vec<(SubstName, Reason, Type, Polarity)>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let InterfaceSig {
        extends,
        props,
        computed_props,
        calls,
        dict,
    } = def;
    let super_ = {
        let super_reason = reason.dupe().update_desc(|d| RSuperOf(Arc::new(d)));
        let mut ts: Vec<Type> = extends
            .iter()
            .map(|t| merge_impl(env, cx, file, t, false, false))
            .collect();
        if !calls.is_empty() {
            ts.insert(
                0,
                Type::new(type_::TypeInner::FunProtoT(super_reason.dupe())),
            );
        }
        match ts.len() {
            0 => Type::new(type_::TypeInner::ObjProtoT(super_reason)),
            1 => ts.into_iter().next().unwrap(),
            _ => {
                let mut iter = ts.into_iter();
                let t0 = iter.next().unwrap();
                let t1 = iter.next().unwrap();
                let rest: Vec<Type> = iter.collect();
                Type::new(type_::TypeInner::IntersectionT(
                    super_reason,
                    type_::inter_rep::make(t0, t1, rest.into()),
                ))
            }
        }
    };
    let static_ = {
        let static_reason = reason.dupe().update_desc(|d| RStatics(Arc::new(d)));
        // interfaces don't have a name field, or even statics
        let mut props_map = BTreeMap::new();
        add_name_field(reason.dupe(), &mut props_map);
        let props = type_::properties::PropertiesMap::from_btree_map(props_map);
        let proto = Type::new(type_::TypeInner::NullProtoT(static_reason.dupe()));
        obj_type::mk_with_proto(
            cx,
            static_reason,
            type_::ObjKind::Inexact,
            None,
            None,
            Some(props),
            None,
            proto,
        )
    };
    let (own_props, proto_props) = {
        let mut own: BTreeMap<Name, type_::Property> = BTreeMap::new();
        let mut proto: BTreeMap<Name, type_::Property> = BTreeMap::new();
        for (k, prop) in props {
            let t = merge_interface_prop(env, cx, file, prop, false);
            let name = Name::new(k.dupe());
            match prop {
                InterfaceProp::InterfaceField(..) => {
                    own.insert(name, t);
                }
                InterfaceProp::InterfaceAccess(..) | InterfaceProp::InterfaceMethod(..) => {
                    proto.insert(name, t);
                }
            }
        }
        for (key, prop) in computed_props {
            if let Some(name) = resolve_computed_key(env, cx, file, key) {
                let t = merge_interface_prop(env, cx, file, prop, false);
                let update_map =
                    |map: &mut BTreeMap<Name, type_::Property>| match map.entry(name.dupe()) {
                        std::collections::btree_map::Entry::Vacant(e) => {
                            e.insert(t.dupe());
                        }
                        std::collections::btree_map::Entry::Occupied(mut e) => {
                            let merged = merge_overloaded_property(e.get(), t.dupe());
                            e.insert(merged);
                        }
                    };
                match prop {
                    InterfaceProp::InterfaceField(..) => update_map(&mut own),
                    InterfaceProp::InterfaceAccess(..) | InterfaceProp::InterfaceMethod(..) => {
                        update_map(&mut proto)
                    }
                }
            }
        }
        (own.into(), proto.into())
    };
    let inst_call_t = {
        let ts: Vec<Type> = calls
            .iter()
            .map(|t| merge_impl(env, cx, file, t, false, false))
            .collect();
        match ts.len() {
            0 => None,
            1 => Some(cx.make_call_prop(ts.into_iter().next().unwrap())),
            _ => {
                let mut iter = ts.into_iter();
                let t0 = iter.next().unwrap();
                let t1 = iter.next().unwrap();
                let rest: Vec<Type> = iter.collect();
                let call_reason = type_util::reason_of_t(&t0).dupe();
                let t = Type::new(type_::TypeInner::IntersectionT(
                    call_reason,
                    type_::inter_rep::make(t0, t1, rest.into()),
                ));
                Some(cx.make_call_prop(t))
            }
        }
    };
    let inst_dict = dict.as_ref().map(|d| merge_dict(env, cx, file, d, false));
    let inst = type_::InstType::new(type_::InstTypeInner {
        class_id: id,
        inst_react_dro: None,
        class_name,
        type_args: targs.into(),
        own_props: cx.generate_property_map(own_props),
        proto_props: cx.generate_property_map(proto_props),
        inst_call_t,
        initialized_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
        initialized_static_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
        inst_kind: type_::InstanceKind::InterfaceKind { inline },
        inst_dict,
        class_private_fields: cx.generate_property_map(type_::properties::PropertiesMap::new()),
        class_private_methods: cx.generate_property_map(type_::properties::PropertiesMap::new()),
        class_private_static_fields: cx
            .generate_property_map(type_::properties::PropertiesMap::new()),
        class_private_static_methods: cx
            .generate_property_map(type_::properties::PropertiesMap::new()),
    });
    Type::new(type_::TypeInner::DefT(
        reason,
        type_::DefT::new(type_::DefTInner::InstanceT(Rc::new(type_::InstanceT::new(
            type_::InstanceTInner {
                inst,
                static_,
                super_,
                implements: vec![].into(),
            },
        )))),
    ))
}

fn merge_class_extends<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    this: Type,
    reason: Reason,
    extends: &ClassExtends<ALoc, Pack::Packed<ALoc>>,
    mixins: &[ClassMixins<ALoc, Pack::Packed<ALoc>>],
) -> (Type, Type) {
    use flow_common::reason::VirtualReasonDesc::*;

    let super_reason = reason.dupe().update_desc(|d| RSuperOf(Arc::new(d)));
    let (super_, static_proto) = match extends {
        ClassExtends::ObjectPrototypeExtendsNull => (
            Type::new(type_::TypeInner::NullProtoT(super_reason.dupe())),
            Type::new(type_::TypeInner::FunProtoT(super_reason.dupe())),
        ),
        ClassExtends::ClassImplicitExtends => (
            Type::new(type_::TypeInner::ObjProtoT(super_reason.dupe())),
            Type::new(type_::TypeInner::FunProtoT(super_reason.dupe())),
        ),
        ClassExtends::ClassExplicitExtends { loc, t } => {
            let reason_op = reason::mk_reason(RClassExtends, loc.dupe());
            let t = specialize(cx, reason_op, merge_impl(env, cx, file, t, false, false));
            let t = type_util::this_typeapp(t, this.dupe(), None, Some(loc.dupe()));
            let static_proto = type_util::class_type(t.dupe(), false, None);
            (t, static_proto)
        }
        ClassExtends::ClassExplicitExtendsApp { loc, t, targs } => {
            let t = merge_impl(env, cx, file, t, false, false);
            let targs: Vec<Type> = targs
                .iter()
                .map(|targ| merge_impl(env, cx, file, targ, false, false))
                .collect();
            let t = type_util::this_typeapp(t, this.dupe(), Some(targs), Some(loc.dupe()));
            let static_proto = type_util::class_type(t.dupe(), false, None);
            (t, static_proto)
        }
    };
    let mut all: Vec<Type> = mixins
        .iter()
        .map(|m| merge_class_mixin(env, cx, file, this.dupe(), m))
        .collect();
    all.push(super_);
    let super_ = match all.len() {
        0 => unreachable!("impossible"),
        1 => all.pop().unwrap(),
        _ => {
            let mut iter = all.into_iter();
            let t0 = iter.next().unwrap();
            let t1 = iter.next().unwrap();
            Type::new(type_::TypeInner::IntersectionT(
                super_reason,
                type_::inter_rep::make(t0, t1, iter.collect::<Vec<_>>().into()),
            ))
        }
    };
    (super_, static_proto)
}

fn merge_class_mixin<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    this: Type,
    mixin: &ClassMixins<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    fn loop_mixin<'cx>(
        cx: &Context<'cx>,
        file: &File<'cx>,
        packed: &Pack::Packed<ALoc>,
    ) -> (Type, Vec<FlowSmolStr>) {
        match packed {
            Pack::Packed::Eval(loc, t, Op::GetProp(name)) => {
                let (t, mut names_rev) = loop_mixin(cx, file, t);
                let t = eval(cx, loc.dupe(), t, Op::GetProp(name.dupe()));
                names_rev.push(name.dupe());
                (t, names_rev)
            }
            Pack::Packed::Ref(r) => {
                let f = |t: Type, _ref_loc: ALoc, _def_loc: ALoc, name: &FlowSmolStr| {
                    (t, vec![name.dupe()])
                };
                merge_ref(cx, file, f, r, false)
            }
            _ => panic!("unexpected class mixin"),
        }
    }

    let merge_mixin_ref =
        |cx: &Context<'cx>, file: &File<'cx>, loc: ALoc, packed: &Pack::Packed<ALoc>| -> Type {
            let (t, names_rev) = loop_mixin(cx, file, packed);
            let names: Vec<&str> = names_rev.iter().map(|s| s.as_str()).collect();
            let name = names.join(".");
            let reason = reason::mk_annot_reason(RType(Name::new(FlowSmolStr::new(&name))), loc);
            annotation_inference::mixin(cx, reason, t)
        };

    match mixin {
        ClassMixins::ClassMixin { loc, t } => {
            let reason_op = reason::mk_reason(RClassMixins, loc.dupe());
            let t = specialize(cx, reason_op, merge_mixin_ref(cx, file, loc.dupe(), t));
            type_util::this_typeapp(t, this, None, Some(loc.dupe()))
        }
        ClassMixins::ClassMixinApp { loc, t, targs } => {
            let t = merge_mixin_ref(cx, file, loc.dupe(), t);
            let targs: Vec<Type> = targs
                .iter()
                .map(|targ| merge_impl(env, cx, file, targ, false, false))
                .collect();
            type_util::this_typeapp(t, this, Some(targs), Some(loc.dupe()))
        }
    }
}

fn merge_this_class_t<'cx>(
    cx: &Context<'cx>,
    file: File<'cx>,
    reason: Reason,
    class_name: Option<FlowSmolStr>,
    id: flow_aloc::ALocId,
    def: ClassSig<ALoc, Pack::Packed<ALoc>>,
    this_reason: Reason,
    inst_kind: type_::InstanceKind,
) -> impl FnOnce(&MergeEnv, Vec<(SubstName, Reason, Type, Polarity)>, Type) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_common::subst_name::SubstName;

    let ClassSig {
        extends,
        implements,
        static_props,
        own_props,
        proto_props,
        dict,
        tparams: _,
    } = def;
    let class_name_owned = class_name;
    move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>, rec_type: Type| {
        let file = &file; // reborrow owned File captured by move
        let this = {
            let this_tp = type_::TypeParam::new(type_::TypeParamInner {
                name: SubstName::name(FlowSmolStr::new_inline("this")),
                reason: this_reason.dupe(),
                bound: rec_type,
                polarity: Polarity::Positive,
                default: None,
                is_this: true,
                is_const: false,
            });
            flow_js_utils::generic_of_tparam(cx, |x| x.dupe(), &this_tp)
        };
        let (super_, static_proto) =
            merge_class_extends(env, cx, file, this.dupe(), reason.dupe(), &extends, &[]);
        let implements: Vec<Type> = implements
            .iter()
            .map(|t| merge_impl(env, cx, file, t, false, false))
            .collect();
        let mut env = env.dupe();
        env.tps.insert(FlowSmolStr::new_inline("this"), this.dupe());
        // let static =
        let static_ = {
            let static_reason = reason.dupe().update_desc(|d| RStatics(Arc::new(d)));
            let mut props_map: BTreeMap<Name, type_::Property> = static_props
                .iter()
                .map(|(k, prop)| {
                    let p = merge_class_prop(&env, cx, file, prop);
                    (Name::new(k.dupe()), p)
                })
                .collect();
            add_name_field(reason.dupe(), &mut props_map);
            let props = type_::properties::PropertiesMap::from_btree_map(props_map);
            obj_type::mk_with_proto(
                cx,
                static_reason,
                type_::ObjKind::Inexact,
                None,
                None,
                Some(props),
                None,
                static_proto,
            )
        };
        let mut own_props_map: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
        for (k, prop) in own_props {
            let p = merge_class_prop(&env, cx, file, &prop);
            own_props_map.insert(k.dupe(), p);
        }
        let mut proto_props_map: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
        for (k, prop) in proto_props {
            let p = merge_class_prop(&env, cx, file, &prop);
            proto_props_map.insert(k.dupe(), p);
        }
        match &inst_kind {
            type_::InstanceKind::RecordKind { defaulted_props } => add_record_constructor(
                cx,
                reason.dupe(),
                class_name_owned.as_ref().unwrap(),
                &own_props_map,
                defaulted_props,
                &mut proto_props_map,
            ),
            _ => {}
        };
        let own_props_pmap: type_::properties::PropertiesMap = own_props_map
            .into_iter()
            .map(|(k, v)| (Name::new(k), v))
            .collect();
        let own_props = cx.generate_property_map(own_props_pmap);
        add_default_constructor(reason.dupe(), &extends, &mut proto_props_map);
        let proto_props_pmap: type_::properties::PropertiesMap = proto_props_map
            .into_iter()
            .map(|(k, v)| (Name::new(k), v))
            .collect();
        let proto_props = cx.generate_property_map(proto_props_pmap);
        let inst = type_::InstType::new(type_::InstTypeInner {
            class_id: id.dupe(),
            inst_react_dro: None,
            class_name: class_name_owned.dupe(),
            type_args: targs.into(),
            own_props,
            proto_props,
            inst_call_t: None,
            initialized_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
            initialized_static_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
            inst_kind,
            inst_dict: dict.as_ref().map(|d| merge_dict(&env, cx, file, d, false)),
            class_private_fields: cx.generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_methods: cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_static_fields: cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_static_methods: cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
        });
        type_util::class_type(
            Type::new(type_::TypeInner::ThisInstanceT(Box::new(
                ThisInstanceTData {
                    reason: reason.dupe(),
                    instance: type_::InstanceT::new(type_::InstanceTInner {
                        inst,
                        static_,
                        super_,
                        implements: implements.into(),
                    }),
                    is_this: false,
                    subst_name: SubstName::name(FlowSmolStr::new_inline("this")),
                },
            ))),
            false,
            None,
        )
    }
}

fn merge_class<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    class_name: Option<&FlowSmolStr>,
    id: flow_aloc::ALocId,
    def: &ClassSig<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let tparams = &def.tparams;
    let this_reason = reason.dupe().replace_desc(RThisType);
    let this_class_t = merge_this_class_t(
        cx,
        file.dupe(),
        reason.dupe(),
        class_name.map(|n| n.dupe()),
        id,
        def.clone(),
        this_reason.dupe(),
        type_::InstanceKind::ClassKind,
    );
    let this_reason_c = this_reason.dupe();
    let t = move |cx: &Context<'cx>,
                  env: &MergeEnv,
                  targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let result_cell: Rc<RefCell<Option<Type>>> = Rc::new(RefCell::new(None));
        let result_cell_c = result_cell.dupe();
        let this_reason_for_lazy = this_reason_c.dupe();
        let env_clone = env.dupe();

        let rec_type = flow_typing_tvar::mk_fully_resolved_lazy(
            cx,
            this_reason_for_lazy,
            false,
            Box::new(move |_cx: &Context| result_cell_c.borrow().as_ref().unwrap().dupe()),
        );
        let class_t = this_class_t(&env_clone, targs, rec_type);
        *result_cell.borrow_mut() = Some(class_t.dupe());
        class_t
    };
    merge_tparams_targs(env, cx, file, reason, t, tparams)
}

fn merge_record<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    record_name: Option<&FlowSmolStr>,
    id: flow_aloc::ALocId,
    def: &ClassSig<ALoc, Pack::Packed<ALoc>>,
    defaulted_props: &BTreeSet<FlowSmolStr>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let tparams = &def.tparams;

    let this_reason = reason.dupe().replace_desc(RThisType);
    let this_class_t = merge_this_class_t(
        cx,
        file.dupe(),
        reason.dupe(),
        record_name.map(|n| n.dupe()),
        id,
        def.clone(),
        this_reason.dupe(),
        type_::InstanceKind::RecordKind {
            defaulted_props: defaulted_props.iter().duped().collect(),
        },
    );
    let this_reason_c = this_reason.dupe();
    let t = move |cx: &Context<'cx>,
                  env: &MergeEnv,
                  targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let result_cell: Rc<RefCell<Option<Type>>> = Rc::new(RefCell::new(None));
        let result_cell_c = result_cell.dupe();
        let this_reason_for_lazy = this_reason_c.dupe();
        let env_clone = env.dupe();

        let rec_type = flow_typing_tvar::mk_fully_resolved_lazy(
            cx,
            this_reason_for_lazy,
            false,
            Box::new(move |_cx: &Context| result_cell_c.borrow().as_ref().unwrap().dupe()),
        );
        let class_t = this_class_t(&env_clone, targs, rec_type);
        *result_cell.borrow_mut() = Some(class_t.dupe());
        class_t
    };
    merge_tparams_targs(env, cx, file, reason, t, tparams)
}

fn merge_fun_statics<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    statics: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>,
) -> Type {
    let props: type_::properties::PropertiesMap = statics
        .iter()
        .map(|(key, (id_loc, t))| {
            let t = merge_impl(env, cx, file, t, false, false);
            let prop =
                type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
                    preferred_def_locs: None,
                    key_loc: Some(id_loc.dupe()),
                    type_: t,
                    polarity: Polarity::Neutral,
                })));
            (Name::new(key.dupe()), prop)
        })
        .collect();
    let reason = reason.update_desc(|d| reason::VirtualReasonDesc::RStatics(Arc::new(d)));
    obj_type::mk_with_proto(
        cx,
        reason.dupe(),
        type_::ObjKind::Inexact,
        None,
        None,
        Some(props),
        None,
        Type::new(type_::TypeInner::FunProtoT(reason)),
    )
}

fn merge_namespace_symbols<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    members: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>,
) -> BTreeMap<Name, NamedSymbol> {
    let mut result: BTreeMap<Name, NamedSymbol> = BTreeMap::new();
    for (name, (loc, packed)) in members {
        let t = merge_impl(env, cx, file, packed, false, false);
        result.insert(
            Name::new(name.dupe()),
            NamedSymbol::new(Some(loc.dupe()), None, t),
        );
    }
    result
}

fn wrap_with_namespace_types<'cx>(
    cx: &Context<'cx>,
    namespace_name: &FlowSmolStr,
    namespace_loc: ALoc,
    values_type: Type,
    namespace_types: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>,
    env: &MergeEnv,
    file: &File<'cx>,
) -> Type {
    if namespace_types.is_empty() {
        values_type
    } else {
        let namespace_symbol =
            Symbol::mk_namespace_symbol(namespace_name.dupe(), namespace_loc.dupe());
        let namespace_types = merge_namespace_symbols(env, cx, file, namespace_types);
        flow_js_utils::namespace_type_with_values_type(
            cx,
            namespace_symbol,
            values_type,
            &namespace_types,
        )
    }
}

fn merge_fun<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    def: &FunSig<ALoc, Pack::Packed<ALoc>>,
    statics: Type,
    is_method: bool,
    is_static: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_type_sig::type_sig;

    let statics = statics.dupe();
    let reason2 = reason.dupe();
    let def_ref = def;
    let t = |_cx: &Context<'cx>,
             env: &MergeEnv,
             _tparam_tuples: Vec<(SubstName, Reason, Type, Polarity)>| {
        let params: Vec<type_::FunParam> = def_ref
            .params
            .iter()
            .map(|param| {
                let t = merge_impl(env, cx, file, &param.t, false, false);
                type_::FunParam(param.name.dupe(), t)
            })
            .collect();
        let rest_param = def_ref.rest_param.as_ref().map(|rp| {
            let t = merge_impl(env, cx, file, &rp.t, false, false);
            type_::FunRestParam(rp.name.dupe(), rp.loc.dupe(), t)
        });
        let this_t = match &def_ref.this_param {
            None => {
                if is_method {
                    type_::implicit_mixed_this(reason2.dupe())
                } else {
                    type_::bound_function_dummy_this(reason2.loc().dupe())
                }
            }
            Some(t) => merge_impl(env, cx, file, t, false, false),
        };
        let return_t = merge_impl(env, cx, file, &def_ref.return_, false, false);
        let type_guard = match &def_ref.type_guard {
            None => None,
            Some(type_sig::TypeGuard {
                loc,
                param_name,
                type_guard: t,
                one_sided,
            }) => {
                let name = &param_name.1;
                let skip_for_rest = match &rest_param {
                    Some(type_::FunRestParam(Some(rest_name), _, _)) if *rest_name == *name => true,
                    _ => false,
                };
                if skip_for_rest
                    || name.as_str() != "this"
                        && params
                            .iter()
                            .all(|p| p.0.as_ref().is_none_or(|pname| *pname != *name))
                    || name.as_str() == "this" && (!is_method || is_static)
                {
                    None
                } else {
                    let tg_reason = reason::mk_reason(RTypeGuard, loc.dupe());
                    Some(type_::TypeGuard::new(type_::TypeGuardInner {
                        reason: tg_reason,
                        one_sided: *one_sided,
                        inferred: false,
                        param_name: param_name.dupe(),
                        type_guard: merge_impl(env, cx, file, t, false, false),
                    }))
                }
            }
        };
        let this_status = if is_method {
            type_::ThisStatus::ThisMethod { unbound: false }
        } else {
            type_::ThisStatus::ThisFunction
        };
        let effect_ = match &def_ref.effect_ {
            type_sig::ReactEffect::HookDecl(l) => {
                type_::ReactEffectType::HookDecl(cx.make_aloc_id(l))
            }
            type_sig::ReactEffect::HookAnnot => type_::ReactEffectType::HookAnnot,
            type_sig::ReactEffect::ArbitraryEffect => type_::ReactEffectType::ArbitraryEffect,
            type_sig::ReactEffect::AnyEffect => type_::ReactEffectType::AnyEffect,
        };
        let funtype = type_::FunType {
            this_t: (this_t, this_status),
            params: params.into(),
            rest_param,
            return_t,
            type_guard,
            def_reason: reason2.dupe(),
            effect_,
        };
        Type::new(type_::TypeInner::DefT(
            reason2.dupe(),
            type_::DefT::new(type_::DefTInner::FunT(statics.dupe(), Rc::new(funtype))),
        ))
    };
    merge_tparams_targs(env, cx, file, reason, t, &def.tparams)
}

fn merge_component<'cx>(
    env: &MergeEnv,
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    is_annotation: bool,
    def: &ComponentSig<ALoc, Pack::Packed<ALoc>>,
    id_opt: Option<(ALoc, &FlowSmolStr)>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    // let t (env, _) =
    let reason2 = reason.dupe();
    let t = |_cx: &Context<'cx>,
             env: &MergeEnv,
             _: Vec<(SubstName, Reason, Type, Polarity)>|
     -> Type {
        let pmap: type_::properties::PropertiesMap = def
            .params
            .iter()
            .map(|param| {
                let ComponentParam { name, name_loc, t } = param;
                let t = merge_impl(env, cx, file, t, false, false);
                (
                    Name::new(name.dupe()),
                    type_::Property::new(type_::PropertyInner::Field(Box::new(type_::FieldData {
                        preferred_def_locs: None,
                        key_loc: Some(name_loc.dupe()),
                        type_: t,
                        polarity: Polarity::Positive,
                    }))),
                )
            })
            .collect();
        let config_reason = reason::mk_reason(
            RPropsOfComponent(Arc::new(reason2.desc(true).clone())),
            def.params_loc.dupe(),
        );
        let rest_t = match &def.rest_param {
            None => obj_type::mk_with_proto(
                cx,
                config_reason.dupe(),
                type_::ObjKind::Exact,
                None,
                None,
                None,
                None,
                Type::new(type_::TypeInner::ObjProtoT(config_reason.dupe())),
            ),
            Some(ComponentRestParam { t }) => merge_impl(env, cx, file, t, false, false),
        };
        let allow_ref_in_spread = match cx.react_ref_as_prop() {
            flow_common::options::ReactRefAsProp::Legacy => is_annotation,
            flow_common::options::ReactRefAsProp::FullSupport => true,
        };
        let param = Type::new(type_::TypeInner::EvalT {
            type_: rest_t,
            defer_use_t: type_::TypeDestructorT::new(type_::TypeDestructorTInner(
                type_::unknown_use(),
                config_reason,
                Rc::new(type_::Destructor::ReactCheckComponentConfig {
                    props: pmap,
                    allow_ref_in_spread,
                }),
            )),
            id: type_::eval::Id::generate_id(),
        });
        let renders = merge_impl(env, cx, file, &def.renders, false, false);
        let component_kind = match id_opt {
            None => type_::ComponentKind::Structural,
            Some((loc, name)) => {
                let id = cx.make_aloc_id(&loc);
                type_::ComponentKind::Nominal(id, name.dupe(), None)
            }
        };
        Type::new(type_::TypeInner::DefT(
            reason2.dupe(),
            type_::DefT::new(type_::DefTInner::ReactAbstractComponentT(Box::new(
                type_::ReactAbstractComponentTData {
                    config: param,
                    renders,
                    component_kind,
                },
            ))),
        ))
    };
    merge_tparams_targs(env, cx, file, reason, t, &def.tparams)
}

fn merge_type_alias<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    custom_error_loc_opt: Option<ALoc>,
    name: &FlowSmolStr,
    tparams: &TParams<ALoc, Pack::Packed<ALoc>>,
    body: &Pack::Packed<ALoc>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let name_owned = name.dupe();
    let reason_clone = reason.dupe();
    let t = move |_cx: &Context<'cx>,
                  env: &MergeEnv,
                  targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let t = merge_impl(env, cx, file, body, false, false);
        match custom_error_loc_opt {
            Some(custom_error_loc) => {
                let id_loc = reason_clone.loc().dupe();
                let name_for_alias = name_owned.dupe();
                let id_loc_for_alias = id_loc.dupe();
                let nominal_reason = reason_clone.dupe().update_desc(move |desc| {
                    RTypeAlias(Box::new((
                        name_for_alias.dupe(),
                        Some(id_loc_for_alias.dupe()),
                        Arc::new(desc),
                    )))
                });
                let id = type_::nominal::Id::UserDefinedOpaqueTypeId(
                    cx.make_aloc_id(&id_loc),
                    name_owned.dupe(),
                );
                let nominal_type = type_::NominalType::new(type_::NominalTypeInner {
                    underlying_t: type_::nominal::UnderlyingT::CustomError {
                        custom_error_loc,
                        t,
                    },
                    lower_t: None,
                    upper_t: None,
                    nominal_id: id,
                    nominal_type_args: targs.into(),
                });
                Type::new(type_::TypeInner::DefT(
                    reason_clone.dupe(),
                    type_::DefT::new(type_::DefTInner::TypeT(
                        type_::TypeTKind::TypeAliasKind,
                        Type::new(type_::TypeInner::NominalT {
                            reason: nominal_reason,
                            nominal_type: Rc::new(nominal_type),
                        }),
                    )),
                ))
            }
            None => {
                let id_loc = reason_clone.loc().dupe();
                let name_for_alias = name_owned.dupe();
                let t = type_util::mod_reason_of_t(
                    &move |reason: Reason| {
                        reason.update_desc(|desc| {
                            RTypeAlias(Box::new((
                                name_for_alias.dupe(),
                                Some(id_loc.dupe()),
                                Arc::new(desc),
                            )))
                        })
                    },
                    &t,
                );
                Type::new(type_::TypeInner::DefT(
                    reason_clone.dupe(),
                    type_::DefT::new(type_::DefTInner::TypeT(type_::TypeTKind::TypeAliasKind, t)),
                ))
            }
        }
    };
    merge_tparams_targs(
        &mk_merge_env(FlowOrdMap::new()),
        cx,
        file,
        reason,
        t,
        tparams,
    )
}

fn merge_opaque_type<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    id: type_::nominal::Id,
    name: &FlowSmolStr,
    tparams: &TParams<ALoc, Pack::Packed<ALoc>>,
    lower_bound: Option<&Pack::Packed<ALoc>>,
    upper_bound: Option<&Pack::Packed<ALoc>>,
    body: Option<&Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    // let t (env, targs) = ...
    let name_owned = name.dupe();
    let reason_clone = reason.dupe();
    let t = move |_cx: &Context<'cx>,
                  env: &MergeEnv,
                  targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let opaque_reason = reason_clone
            .dupe()
            .replace_desc(ROpaqueType(name_owned.dupe()));
        let lower_t = lower_bound.map(|t| merge_impl(env, cx, file, t, false, false));
        let upper_t = upper_bound.map(|t| merge_impl(env, cx, file, t, false, false));
        let underlying_t = match body {
            None => type_::nominal::UnderlyingT::FullyOpaque,
            Some(t) => type_::nominal::UnderlyingT::OpaqueWithLocal {
                t: merge_impl(env, cx, file, t, false, false),
            },
        };
        let nominal_type = type_::NominalType::new(type_::NominalTypeInner {
            underlying_t,
            lower_t,
            upper_t,
            nominal_id: id.clone(),
            nominal_type_args: targs.into(),
        });
        Type::new(type_::TypeInner::DefT(
            reason_clone.dupe(),
            type_::DefT::new(type_::DefTInner::TypeT(
                type_::TypeTKind::OpaqueKind,
                Type::new(type_::TypeInner::NominalT {
                    reason: opaque_reason,
                    nominal_type: Rc::new(nominal_type),
                }),
            )),
        ))
    };
    merge_tparams_targs(
        &mk_merge_env(FlowOrdMap::new()),
        cx,
        file,
        reason,
        t,
        tparams,
    )
}

fn merge_declare_class<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    class_name: &FlowSmolStr,
    id: flow_aloc::ALocId,
    def: &DeclareClassSig<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_common::subst_name::SubstName;

    let DeclareClassSig {
        tparams,
        extends,
        mixins,
        implements,
        static_props,
        own_props,
        proto_props,
        computed_own_props,
        computed_proto_props,
        computed_static_props,
        static_calls,
        calls,
        dict,
        static_dict: _,
    } = def;
    let this_reason = reason.dupe().replace_desc(RThisType);
    let class_name_owned = class_name.dupe();
    let id_owned = id.dupe();
    let reason_c = reason.dupe();
    let this_reason_c = this_reason.dupe();
    let file_c = file.dupe();
    let this_class_t = move |env: &MergeEnv,
                             targs: Vec<(SubstName, Reason, Type, Polarity)>,
                             rec_type: Type| {
        let file = &file_c;
        let this = {
            let this_tp = type_::TypeParam::new(type_::TypeParamInner {
                name: SubstName::name(FlowSmolStr::new_inline("this")),
                reason: this_reason_c.dupe(),
                bound: rec_type,
                polarity: Polarity::Positive,
                default: None,
                is_this: true,
                is_const: false,
            });
            flow_js_utils::generic_of_tparam(cx, |x| x.dupe(), &this_tp)
        };
        let (super_, static_proto) =
            merge_class_extends(env, cx, file, this.dupe(), reason_c.dupe(), extends, mixins);
        let implements: Vec<Type> = implements
            .iter()
            .map(|t| merge_impl(env, cx, file, t, false, false))
            .collect();
        let mut env = env.dupe();
        env.tps.insert(FlowSmolStr::new_inline("this"), this.dupe());
        let static_ = {
            let static_reason = reason_c.dupe().update_desc(|d| RStatics(Arc::new(d)));
            let mut props_smap: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
            for (k, prop) in static_props {
                let p = merge_interface_prop(&env, cx, file, prop, true);
                props_smap.insert(k.dupe(), p);
            }
            for (key, prop) in computed_static_props {
                if let Some(name) = resolve_computed_key(&env, cx, file, key) {
                    let name_str = name.into_smol_str();
                    let t = merge_interface_prop(&env, cx, file, prop, true);
                    match props_smap.entry(name_str) {
                        std::collections::btree_map::Entry::Vacant(e) => {
                            e.insert(t);
                        }
                        std::collections::btree_map::Entry::Occupied(mut e) => {
                            let merged = merge_overloaded_property(e.get(), t);
                            e.insert(merged);
                        }
                    }
                }
            }
            let mut props_map: BTreeMap<Name, type_::Property> = props_smap
                .into_iter()
                .map(|(k, v)| (Name::new(k), v))
                .collect();
            add_name_field(reason_c.dupe(), &mut props_map);
            let props = type_::properties::PropertiesMap::from_btree_map(props_map);
            let call = {
                let ts: Vec<Type> = static_calls
                    .iter()
                    .map(|t| merge_impl(&env, cx, file, t, false, false))
                    .collect();
                match ts.len() {
                    0 => None,
                    1 => Some(ts.into_iter().next().unwrap()),
                    _ => {
                        let mut iter = ts.into_iter();
                        let t0 = iter.next().unwrap();
                        let t1 = iter.next().unwrap();
                        let rest: Vec<Type> = iter.collect();
                        let r = type_util::reason_of_t(&t0).dupe();
                        Some(Type::new(type_::TypeInner::IntersectionT(
                            r,
                            type_::inter_rep::make(t0, t1, rest.into()),
                        )))
                    }
                }
            };
            obj_type::mk_with_proto(
                cx,
                static_reason,
                type_::ObjKind::Inexact,
                None,
                call,
                Some(props),
                None,
                static_proto,
            )
        };
        let fold_computed_interface_props =
            |computed_props: &[(Pack::Packed<ALoc>, InterfaceProp<ALoc, Pack::Packed<ALoc>>)],
             props: &mut BTreeMap<FlowSmolStr, type_::Property>| {
                for (key, prop) in computed_props {
                    if let Some(name) = resolve_computed_key(&env, cx, file, key) {
                        let name_str = name.into_smol_str();
                        let t = merge_interface_prop(&env, cx, file, prop, false);
                        match props.entry(name_str) {
                            std::collections::btree_map::Entry::Vacant(e) => {
                                e.insert(t);
                            }
                            std::collections::btree_map::Entry::Occupied(mut e) => {
                                let merged = merge_overloaded_property(e.get(), t);
                                e.insert(merged);
                            }
                        }
                    }
                }
            };
        let own_props = {
            let mut props: BTreeMap<FlowSmolStr, type_::Property> = own_props
                .iter()
                .map(|(k, prop)| {
                    let p = merge_interface_prop(&env, cx, file, prop, false);
                    (k.dupe(), p)
                })
                .collect();
            fold_computed_interface_props(computed_own_props, &mut props);
            let pmap: type_::properties::PropertiesMap =
                props.into_iter().map(|(k, v)| (Name::new(k), v)).collect();
            cx.generate_property_map(pmap)
        };
        let proto_props = {
            let mut proto_map: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
            for (k, prop) in proto_props {
                let p = merge_interface_prop(&env, cx, file, prop, false);
                proto_map.insert(k.dupe(), p);
            }
            add_default_constructor(reason_c.dupe(), extends, &mut proto_map);
            fold_computed_interface_props(computed_proto_props, &mut proto_map);
            let pmap: type_::properties::PropertiesMap = proto_map
                .into_iter()
                .map(|(k, v)| (Name::new(k), v))
                .collect();
            cx.generate_property_map(pmap)
        };
        let inst_call_t = {
            let ts: Vec<Type> = calls
                .iter()
                .map(|t| merge_impl(&env, cx, file, t, false, false))
                .collect();
            match ts.len() {
                0 => None,
                1 => Some(cx.make_call_prop(ts.into_iter().next().unwrap())),
                _ => {
                    let mut iter = ts.into_iter();
                    let t0 = iter.next().unwrap();
                    let t1 = iter.next().unwrap();
                    let rest: Vec<Type> = iter.collect();
                    let r = type_util::reason_of_t(&t0).dupe();
                    let t = Type::new(type_::TypeInner::IntersectionT(
                        r,
                        type_::inter_rep::make(t0, t1, rest.into()),
                    ));
                    Some(cx.make_call_prop(t))
                }
            }
        };
        let inst_dict = dict.as_ref().map(|d| merge_dict(&env, cx, file, d, false));
        let inst = type_::InstType::new(type_::InstTypeInner {
            class_id: id_owned.dupe(),
            inst_react_dro: None,
            class_name: Some(class_name_owned.dupe()),
            type_args: targs.into(),
            own_props,
            proto_props,
            inst_call_t,
            initialized_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
            initialized_static_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
            inst_kind: type_::InstanceKind::ClassKind,
            inst_dict,
            class_private_fields: cx.generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_methods: cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_static_fields: cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_static_methods: cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
        });
        type_util::class_type(
            Type::new(type_::TypeInner::ThisInstanceT(Box::new(
                ThisInstanceTData {
                    reason: reason_c.dupe(),
                    instance: type_::InstanceT::new(type_::InstanceTInner {
                        inst,
                        static_,
                        super_,
                        implements: implements.into(),
                    }),
                    is_this: false,
                    subst_name: SubstName::name(FlowSmolStr::new_inline("this")),
                },
            ))),
            false,
            None,
        )
    };
    let this_reason_t = this_reason.dupe();
    let t = move |_cx: &Context<'cx>,
                  env: &MergeEnv,
                  targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let result_cell: Rc<RefCell<Option<Type>>> = Rc::new(RefCell::new(None));
        let result_cell_c = result_cell.dupe();
        let this_reason_for_lazy = this_reason_t.dupe();
        let env_clone = env.dupe();

        let rec_type = flow_typing_tvar::mk_fully_resolved_lazy(
            _cx,
            this_reason_for_lazy,
            false,
            Box::new(move |_cx: &Context| result_cell_c.borrow().as_ref().unwrap().dupe()),
        );
        let class_t = this_class_t(&env_clone, targs, rec_type);
        *result_cell.borrow_mut() = Some(class_t.dupe());
        class_t
    };
    merge_tparams_targs(
        &mk_merge_env(FlowOrdMap::new()),
        cx,
        file,
        reason,
        t,
        tparams,
    )
}

fn merge_declare_fun<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    name: &FlowSmolStr,
    id_loc: ALoc,
    defs: &Vec1<(ALoc, ALoc, FunSig<ALoc, Pack::Packed<ALoc>>)>,
    statics: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>,
    namespace_types: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    let env = mk_merge_env(FlowOrdMap::new());
    let statics_t = merge_fun_statics(&env, cx, file, reason.dupe(), statics);
    let ts: Vec<Type> = defs
        .iter()
        .map(|(_, fn_loc, def)| {
            let reason = reason::mk_reason(RFunctionType, fn_loc.dupe());
            merge_fun(&env, cx, file, reason, def, statics_t.dupe(), false, false)
        })
        .collect();
    let function_t = match ts.len() {
        1 => ts.into_iter().next().unwrap(),
        _ => {
            let mut iter = ts.into_iter();
            let t0 = iter.next().unwrap();
            let t1 = iter.next().unwrap();
            let rest: Vec<Type> = iter.collect();
            let reason = type_util::reason_of_t(&t0)
                .dupe()
                .replace_desc(RIntersectionType);
            Type::new(type_::TypeInner::IntersectionT(
                reason,
                type_::inter_rep::make(t0, t1, rest.into()),
            ))
        }
    };
    wrap_with_namespace_types(cx, name, id_loc, function_t, namespace_types, &env, file)
}

pub fn merge_def<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    reason: Reason,
    def: &Def<ALoc, Pack::Packed<ALoc>>,
    const_decl: bool,
) -> Type {
    match def {
        Def::TypeAlias(inner) => merge_type_alias(
            cx,
            file,
            reason,
            inner.custom_error_loc_opt.dupe(),
            &inner.name,
            &inner.tparams,
            &inner.body,
        ),
        Def::OpaqueType(inner) => {
            let id = type_::nominal::Id::UserDefinedOpaqueTypeId(
                cx.make_aloc_id(&inner.id_loc),
                inner.name.dupe(),
            );
            merge_opaque_type(
                cx,
                file,
                reason,
                id,
                &inner.name,
                &inner.tparams,
                inner.lower_bound.as_ref(),
                inner.upper_bound.as_ref(),
                inner.body.as_ref(),
            )
        }
        Def::Interface(inner) => {
            let id = cx.make_aloc_id(&inner.id_loc);
            let name = &inner.name;
            let def = &inner.def;
            let tparams = &inner.tparams;
            let t = |_cx: &Context<'cx>,
                     env: &MergeEnv,
                     targs: Vec<(SubstName, Reason, Type, Polarity)>| {
                let t = merge_interface(
                    env,
                    cx,
                    file,
                    reason.dupe(),
                    Some(name.dupe()),
                    id.dupe(),
                    def,
                    false,
                    targs,
                );
                type_util::class_type(t, false, None)
            };
            merge_tparams_targs(
                &mk_merge_env(FlowOrdMap::new()),
                cx,
                file,
                reason.dupe(),
                t,
                tparams,
            )
        }
        Def::ClassBinding(inner) => {
            let id = cx.make_aloc_id(&inner.id_loc);
            merge_class(
                &mk_merge_env(FlowOrdMap::new()),
                cx,
                file,
                reason,
                Some(&inner.name),
                id,
                &inner.def,
            )
        }
        Def::DeclareClassBinding(inner) => {
            let nominal_id = cx.make_aloc_id(&inner.nominal_id_loc);
            merge_declare_class(cx, file, reason, &inner.name, nominal_id, &inner.def)
        }
        Def::RecordBinding(inner) => {
            let id = cx.make_aloc_id(&inner.id_loc);
            merge_record(
                &mk_merge_env(FlowOrdMap::new()),
                cx,
                file,
                reason,
                Some(&inner.name),
                id,
                &inner.def,
                &inner.defaulted_props,
            )
        }
        Def::FunBinding(inner) => {
            let env = mk_merge_env(FlowOrdMap::new());
            let statics_t = merge_fun_statics(&env, cx, file, reason.dupe(), &inner.statics);
            let function_t = merge_fun(
                &env,
                cx,
                file,
                reason.dupe(),
                &inner.def,
                statics_t,
                false,
                false,
            );
            wrap_with_namespace_types(
                cx,
                &inner.name,
                inner.id_loc.dupe(),
                function_t,
                &inner.namespace_types,
                &env,
                file,
            )
        }
        Def::DeclareFun(inner) => {
            let mut all_defs = vec![(inner.id_loc.dupe(), inner.fn_loc.dupe(), inner.def.clone())];
            all_defs.extend(inner.tail.iter().cloned());
            let defs = Vec1::try_from_vec(all_defs).unwrap();
            merge_declare_fun(
                cx,
                file,
                reason,
                &inner.name,
                inner.id_loc.dupe(),
                &defs,
                &inner.statics,
                &inner.namespace_types,
            )
        }
        Def::ComponentBinding(inner) => {
            let env = mk_merge_env(FlowOrdMap::new());
            merge_component(
                &env,
                cx,
                file,
                reason,
                false,
                &inner.def,
                Some((inner.id_loc.dupe(), &inner.name)),
            )
        }
        Def::Variable(inner) => {
            let env = mk_merge_env(FlowOrdMap::new());
            merge_impl(&env, cx, file, &inner.def, false, const_decl)
        }
        Def::Parameter(inner) => {
            let def = &inner.def;
            let tparams = &inner.tparams;
            let t = |_cx: &Context<'cx>,
                     env: &MergeEnv,
                     _targs: Vec<(SubstName, Reason, Type, Polarity)>| {
                merge_impl(env, cx, file, def, false, const_decl)
            };
            merge_tparams_targs(
                &mk_merge_env(FlowOrdMap::new()),
                cx,
                file,
                reason,
                t,
                tparams,
            )
        }
        Def::DisabledComponentBinding(_)
        | Def::DisabledEnumBinding(_)
        | Def::DisabledRecordBinding(_) => type_::any_t::error(reason),
        Def::EnumBinding(inner) if inner.rep.is_none() => type_::any_t::error(reason),
        Def::EnumBinding(inner) => {
            let rep = inner.rep.as_ref().unwrap();
            merge_enum(
                cx,
                reason,
                inner.id_loc.dupe(),
                &inner.name,
                rep,
                &inner.members,
                inner.has_unknown_members,
            )
        }
        Def::NamespaceBinding(inner) => {
            let (id_loc, name, values, types) =
                (&inner.id_loc, &inner.name, &inner.values, &inner.types);
            let env = mk_merge_env(FlowOrdMap::new());
            let namespace_symbol = Symbol::mk_namespace_symbol(name.dupe(), id_loc.dupe());
            flow_js_utils::namespace_type(
                cx,
                reason,
                namespace_symbol,
                &merge_namespace_symbols(&env, cx, file, values),
                &merge_namespace_symbols(&env, cx, file, types),
            )
        }
    }
}

pub fn merge_export<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    export: &Pack::Export<ALoc>,
) -> NamedSymbol {
    match export {
        Pack::Export::ExportRef(ref_) => merge_ref(
            cx,
            file,
            |type_, _ref_loc, def_loc, _name| NamedSymbol::new(Some(def_loc), None, type_),
            ref_,
            false,
        ),
        Pack::Export::ExportDefault { default_loc, def } => {
            if let Pack::Packed::Ref(ref_) = def {
                merge_ref(
                    cx,
                    file,
                    |type_, _ref_loc, def_loc, _name| NamedSymbol::new(Some(def_loc), None, type_),
                    ref_,
                    false,
                )
            } else {
                let type_ = merge_impl(
                    &mk_merge_env(FlowOrdMap::new()),
                    cx,
                    file,
                    def,
                    false,
                    false,
                );
                NamedSymbol::new(Some(default_loc.dupe()), None, type_)
            }
        }
        Pack::Export::ExportBinding(index) => {
            let __rc = file.local_defs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (loc, _name, _t_general, t_const) = entry;
            let type_ = t_const.get_forced(cx).dupe();
            NamedSymbol::new(Some(loc.dupe()), None, type_)
        }
        Pack::Export::ExportDefaultBinding {
            default_loc: _,
            index,
        } => {
            let __rc = file.local_defs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (loc, _name, _t_general, t_const) = entry;
            let type_ = t_const.get_forced(cx).dupe();
            NamedSymbol::new(Some(loc.dupe()), None, type_)
        }
        Pack::Export::ExportFrom(index) => {
            let __rc = file.remote_refs.borrow().get(*index).dupe();
            let entry = __rc.get_forced(cx);
            let (loc, _name, type_) = entry;
            NamedSymbol::new(Some(loc.dupe()), None, type_.dupe())
        }
    }
}

pub fn merge_resource_module_t<'cx>(
    cx: &Context<'cx>,
    file_key: FileKey,
    filename: &str,
) -> (Reason, LazyModuleType<'cx>) {
    use flow_common::reason::VirtualReasonDesc::*;

    let exports_t = match std::path::Path::new(filename)
        .extension()
        .and_then(|s| s.to_str())
    {
        Some("css") => {
            let reason = reason::mk_reason(RObjectType, ALoc::none());
            type_::any_t::make(type_::AnySource::Untyped, reason)
        }
        Some(_) => {
            let reason = reason::mk_reason(RString, ALoc::none());
            type_::str_module_t::why(reason)
        }
        None => panic!("How did we find a resource file without an extension?!"),
    };
    let file_loc = ALoc::of_loc(Loc {
        source: Some(file_key),
        ..LOC_NONE
    });
    let reason = reason::mk_reason(RExports, file_loc);
    (
        reason.dupe(),
        mk_commonjs_module_t(cx, reason, cx.is_strict(), None, None, exports_t),
    )
}

pub fn merge<'cx>(
    tps: TparamsMap,
    cx: &Context<'cx>,
    file: &File<'cx>,
    packed: &Pack::Packed<ALoc>,
) -> Type {
    let env = mk_merge_env(tps);
    merge_impl(&env, cx, file, packed, false, false)
}

pub fn merge_cjs_export_t<'cx>(
    cx: &Context<'cx>,
    file: &File<'cx>,
    packed: &Pack::Packed<ALoc>,
) -> (Option<ALoc>, Type) {
    use flow_type_sig::type_sig::Value;
    // We run a special code path for objects in cjs exports,
    // in order to retain the original definition location of exported names
    match packed {
        Pack::Packed::Value(value) => match value.as_ref() {
            Value::ObjLit(inner) => {
                let env = mk_merge_env(FlowOrdMap::new());
                (
                    Some(inner.loc.dupe()),
                    merge_object_lit(
                        &env,
                        cx,
                        file,
                        inner.loc.dupe(),
                        inner.frozen,
                        &inner.proto,
                        &inner.props,
                        true,
                        false,
                    ),
                )
            }
            Value::ObjSpreadLit(inner) => {
                let env = mk_merge_env(FlowOrdMap::new());
                (
                    Some(inner.loc.dupe()),
                    merge_obj_spread_lit(
                        &env,
                        cx,
                        file,
                        inner.loc.dupe(),
                        inner.frozen,
                        &inner.proto,
                        &inner.elems,
                        true,
                        false,
                    ),
                )
            }
            _ => (None, merge(FlowOrdMap::new(), cx, file, packed)),
        },
        Pack::Packed::Ref(ref_) => merge_ref(
            cx,
            file,
            |type_, _ref_loc, def_loc, _name| (Some(def_loc), type_),
            ref_,
            false,
        ),
        _ => (None, merge(FlowOrdMap::new(), cx, file, packed)),
    }
}

pub fn merge_builtins<'cx>(
    cx: &Context<'cx>,
    file_key: FileKey,
    builtin_locs: Arc<Table<Loc>>,
    builtins: Arc<packed_type_sig::Builtins<Loc>>,
) -> (
    BTreeMap<FlowSmolStr, flow_typing_builtins::LazyVal<'cx, Context<'cx>>>,
    BTreeMap<FlowSmolStr, flow_typing_builtins::LazyVal<'cx, Context<'cx>>>,
    BTreeMap<FlowSmolStr, flow_typing_builtins::LazyModule<'cx, Context<'cx>>>,
) {
    let source = Some(file_key.dupe());
    let aloc_table: flow_aloc::LazyALocTable = Rc::new(LazyCell::new(Box::new({
        let file_key = file_key.dupe();
        let builtin_locs = builtin_locs.dupe();
        move || {
            Rc::new(aloc_representation_do_not_use::make_table(
                file_key,
                builtin_locs.iter().cloned().collect(),
            ))
        }
    })
        as Box<dyn FnOnce() -> Rc<ALocTable>>));
    let aloc = {
        let source = source.dupe();
        let aloc_table = aloc_table.dupe();
        move |i: &Index<Loc>| -> ALoc {
            ALoc::of_loc(
                aloc_representation_do_not_use::make_keyed(source.dupe(), i.as_usize() as u32)
                    .to_loc(&aloc_table),
            )
        }
    };
    let aloc = Rc::new(aloc);

    type FileAndDepMap<'a> = (
        File<'a>,
        BTreeMap<FlowSmolStr, flow_typing_builtins::LazyModule<'a, Context<'a>>>,
    );
    type LazyFileAndDepMap<'a> = Rc<
        flow_lazy::Lazy<
            Context<'a>,
            FileAndDepMap<'a>,
            Box<dyn FnOnce(&Context<'a>) -> FileAndDepMap<'a> + 'a>,
        >,
    >;

    fn local_def_impl<'cx>(
        aloc: &Rc<impl Fn(&Index<Loc>) -> ALoc + 'static>,
        file_and_dependency_map_rec: &LazyFileAndDepMap<'cx>,
        def: &Pack::PackedDef<Index<Loc>>,
    ) -> Rc<
        flow_lazy::Lazy<
            Context<'cx>,
            (
                ALoc,
                FlowSmolStr,
                Rc<
                    flow_lazy::Lazy<
                        Context<'cx>,
                        Type,
                        Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                    >,
                >,
                Rc<
                    flow_lazy::Lazy<
                        Context<'cx>,
                        Type,
                        Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                    >,
                >,
            ),
            Box<
                dyn FnOnce(
                        &Context<'cx>,
                    ) -> (
                        ALoc,
                        FlowSmolStr,
                        Rc<
                            flow_lazy::Lazy<
                                Context<'cx>,
                                Type,
                                Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                            >,
                        >,
                        Rc<
                            flow_lazy::Lazy<
                                Context<'cx>,
                                Type,
                                Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                            >,
                        >,
                    ) + 'cx,
            >,
        >,
    > {
        let aloc = aloc.dupe();
        let def = def.clone();
        let file_and_dep = file_and_dependency_map_rec.dupe();
        Rc::new(flow_lazy::Lazy::new(Box::new(move |_cx: &Context<'cx>| {
            let def = Rc::new(def.map(
                &mut (),
                |_, loc: &Index<Loc>| (*aloc)(loc),
                |_, t: &Pack::Packed<Index<Loc>>| t.map(&|i| (*aloc)(i)),
            ));
            let loc = def.id_loc();
            let name = def.name().dupe();
            let reason = def_reason(&def);
            let make_type = |const_decl: bool| -> Rc<
                flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>,
            > {
                let file_and_dep = file_and_dep.dupe();
                let reason = reason.dupe();
                let reason_for_tvar = reason.dupe();
                let def = def.dupe();
                Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                    let once_resolved: Rc<
                        flow_lazy::Lazy<
                            Context<'cx>,
                            Type,
                            Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                        >,
                    > = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        let (file, _) = file_and_dep.get_forced(cx);
                        merge_def(cx, file, reason, &def, const_decl)
                    })));
                    let resolved =
                        Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                            once_resolved.get_forced(cx).dupe()
                        })
                            as Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>));
                    annotation_inference::mk_sig_tvar(cx, reason_for_tvar, resolved)
                })))
            };
            (loc, name, make_type(false), make_type(true))
        })
            as Box<
                dyn FnOnce(
                        &Context<'cx>,
                    ) -> (
                        ALoc,
                        FlowSmolStr,
                        Rc<
                            flow_lazy::Lazy<
                                Context<'cx>,
                                Type,
                                Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                            >,
                        >,
                        Rc<
                            flow_lazy::Lazy<
                                Context<'cx>,
                                Type,
                                Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                            >,
                        >,
                    ) + 'cx,
            >))
    }

    let remote_ref_fn = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap<'cx>,
              rref: &Pack::RemoteRef<Index<Loc>>|
              -> Rc<
            flow_lazy::Lazy<
                Context<'cx>,
                (ALoc, FlowSmolStr, Type),
                Box<dyn FnOnce(&Context<'cx>) -> (ALoc, FlowSmolStr, Type) + 'cx>,
            >,
        > {
            let aloc = aloc.dupe();
            let rref = rref.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                let rref = rref.map(&|i| (*aloc)(i));
                let loc = rref.loc().dupe();
                let name = rref.name().dupe();
                let reason = remote_ref_reason(&rref);
                let file_and_dep2 = file_and_dep.dupe();
                let reason2 = reason.dupe();
                let rref2 = rref.clone();
                let resolved: Rc<
                    flow_lazy::Lazy<
                        Context<'cx>,
                        Type,
                        Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                    >,
                > = Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                    let (file, _) = file_and_dep2.get_forced(cx);
                    merge_remote_ref(cx, file, reason2, &rref2)
                })));
                let t = annotation_inference::mk_sig_tvar(cx, reason.dupe(), resolved);
                (loc, name, t)
            })
                as Box<
                    dyn FnOnce(&Context<'cx>) -> (ALoc, FlowSmolStr, Type) + 'cx,
                >))
        }
    };

    let pattern_def_fn = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap<'cx>,
              def: &Pack::Packed<Index<Loc>>|
              -> Rc<
            flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>,
        > {
            let aloc = aloc.dupe();
            let def = def.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                let def = def.map(&|i| (*aloc)(i));
                let (file, _) = file_and_dep.get_forced(cx);
                merge(FlowOrdMap::new(), cx, file, &def)
            })
                as Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>))
        }
    };

    let pattern_fn = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap<'cx>,
              p: &Pack::Pattern<Index<Loc>>|
              -> Rc<
            flow_lazy::Lazy<Context<'cx>, Type, Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>>,
        > {
            let aloc = aloc.dupe();
            let p = p.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                let p = p.map(&|i| (*aloc)(i));
                let (file, _) = file_and_dep.get_forced(cx);
                merge_pattern(cx, file, &p)
            })
                as Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>))
        }
    };

    let map_module = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap<'cx>,
              module_loc: ALoc,
              module_kind: &Pack::ModuleKind<Index<Loc>>|
              -> flow_typing_builtins::LazyModule<'cx, Context<'cx>> {
            use flow_common::reason::VirtualReasonDesc::*;
            let reason = reason::mk_reason(RExports, module_loc);

            let aloc_fn = aloc.dupe();
            let module_kind = module_kind.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            let reason2 = reason.dupe();
            Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                let type_export = |export: &Pack::TypeExport<Index<Loc>>| -> LazyExport<'cx> {
                    let export = export.clone();
                    let aloc_fn = aloc_fn.dupe();
                    let file_and_dep = file_and_dep.dupe();
                    let reason = reason2.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        let export = export.map(&*aloc_fn);
                        let (file, _) = file_and_dep.get_forced(cx);
                        merge_type_export(cx, file, reason, &export)
                    })))
                };
                let cjs_exports_fn = |export: Pack::Packed<Index<Loc>>| -> LazyCjsExport<'cx> {
                    let aloc_fn = aloc_fn.dupe();
                    let file_and_dep = file_and_dep.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        let export = export.map(&*aloc_fn);
                        let (file, _) = file_and_dep.get_forced(cx);
                        merge_cjs_export_t(cx, file, &export)
                    })))
                };
                let es_export_fn = |export: &Pack::Export<Index<Loc>>| -> LazyExport<'cx> {
                    let export = export.clone();
                    let aloc_fn = aloc_fn.dupe();
                    let file_and_dep = file_and_dep.dupe();
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        let export = export.map(&*aloc_fn);
                        let (file, _) = file_and_dep.get_forced(cx);
                        merge_export(cx, file, &export)
                    })))
                };
                let cjs_module = |type_exports: &[Pack::TypeExport<Index<Loc>>],
                                  exports: Option<Pack::Packed<Index<Loc>>>,
                                  info: Pack::CJSModuleInfo<Index<Loc>>|
                 -> Exports<'cx> {
                    let info = info.map(&*aloc_fn);
                    let Pack::CJSModuleInfo {
                        type_export_keys,
                        type_stars,
                        strict,
                        platform_availability_set,
                    } = info;
                    let type_exports = type_exports.iter().map(&type_export).collect::<Vec<_>>();
                    let exports = exports.map(&cjs_exports_fn);
                    let type_exports: BTreeMap<FlowSmolStr, LazyExport<'cx>> =
                        type_export_keys.into_iter().zip(type_exports).collect();
                    Exports::CJSExports {
                        type_exports,
                        exports,
                        type_stars,
                        strict,
                        platform_availability_set,
                    }
                };
                let es_module = |type_exports: &[Pack::TypeExport<Index<Loc>>],
                                 exports: &[Pack::Export<Index<Loc>>],
                                 info: Pack::ESModuleInfo<Index<Loc>>|
                 -> Exports<'cx> {
                    let info = info.map(&*aloc_fn);
                    let Pack::ESModuleInfo {
                        type_export_keys,
                        export_keys,
                        type_stars,
                        stars,
                        strict,
                        platform_availability_set,
                    } = info;
                    let type_exports = type_exports.iter().map(&type_export).collect::<Vec<_>>();
                    let type_exports: BTreeMap<FlowSmolStr, LazyExport<'cx>> =
                        type_export_keys.into_iter().zip(type_exports).collect();
                    let exports = exports.iter().map(es_export_fn).collect::<Vec<_>>();
                    let exports: BTreeMap<FlowSmolStr, LazyExport<'cx>> =
                        export_keys.into_iter().zip(exports).collect();
                    Exports::ESExports {
                        type_exports,
                        exports,
                        type_stars,
                        stars,
                        strict,
                        platform_availability_set,
                    }
                };
                let info = match module_kind {
                    Pack::ModuleKind::CJSModule {
                        type_exports,
                        exports,
                        info,
                    } => cjs_module(&type_exports, exports, info),
                    Pack::ModuleKind::ESModule {
                        type_exports,
                        exports,
                        info,
                    } => es_module(&type_exports, &exports, info),
                };
                let (file, _) = file_and_dep.get_forced(cx);
                let reason2_ret = reason2.dupe();
                let lazy_module = merge_exports(cx, file, reason2, info);
                (reason2_ret, lazy_module)
            })
                as Box<
                    dyn FnOnce(
                            &Context<'cx>,
                        ) -> (
                            Reason,
                            flow_typing_builtins::LazyModuleType<'cx, Context<'cx>>,
                        ) + 'cx,
                >))
        }
    };

    let file_and_dependency_map_rec: LazyFileAndDepMap<'cx> = {
        use std::cell::OnceCell;
        let cell: Rc<OnceCell<FileAndDepMap<'cx>>> = Rc::new(OnceCell::new());
        let cell_ref = cell.dupe();
        let aloc = aloc.dupe();
        let builtins = builtins.dupe();
        Rc::new(flow_lazy::Lazy::new(Box::new(move |_cx: &Context<'cx>| {
            if let Some(val) = cell_ref.get() {
                return (val.0.dupe(), val.1.clone());
            }
            let self_ref: LazyFileAndDepMap<'cx> = {
                let cell_ref2 = cell_ref.dupe();
                Rc::new(flow_lazy::Lazy::new(Box::new(move |_cx: &Context<'cx>| {
                    let val = cell_ref2.get().expect("recursive lazy not yet initialized");
                    (val.0.dupe(), val.1.clone())
                })
                    as Box<dyn FnOnce(&Context<'cx>) -> FileAndDepMap<'cx> + 'cx>))
            };

            let mut dependencies_map: BTreeMap<
                FlowSmolStr,
                flow_typing_builtins::LazyModule<'cx, Context<'cx>>,
            > = BTreeMap::new();
            for (module_name, module_def) in builtins.global_modules.iter() {
                let module_loc = (*aloc)(&module_def.loc);
                let lazy_module = map_module(&self_ref, module_loc, &module_def.module_kind);
                // SMap.add module_name lazy_module acc
                dependencies_map.insert(module_name.dupe(), lazy_module);
            }

            let map_module_ref = |specifier: &Userland,
                                  dependencies_map: &BTreeMap<
                FlowSmolStr,
                flow_typing_builtins::LazyModule<'cx, Context<'cx>>,
            >|
             -> Rc<
                flow_lazy::Lazy<
                    Context<'cx>,
                    ResolvedRequire<'cx>,
                    Box<dyn FnOnce(&Context<'cx>) -> ResolvedRequire<'cx> + 'cx>,
                >,
            > {
                match dependencies_map.get(specifier.as_str()) {
                    None => Rc::new(flow_lazy::Lazy::new(Box::new(|_cx: &Context| {
                        ResolvedRequire::MissingModule
                    }))),
                    Some(lazy_module) => {
                        let lazy_module = lazy_module.dupe();
                        Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                            let (r, lazy_module) = lazy_module.get_forced(cx);
                            let s =
                                type_::constraint::forcing_state::ModuleTypeForcingState::of_lazy_module(
                                    r.dupe(),
                                    lazy_module.dupe(),
                                );
                            ResolvedRequire::TypedModule(
                                annotation_inference::force_module_type_thunk(cx.dupe(), s),
                            )
                        })))
                    }
                }
            };

            use flow_common::reason::VirtualReasonDesc::*;
            let exports: Rc<
                dyn Fn(&Context<'cx>, &Context<'cx>) -> Result<ModuleType, Type> + 'cx,
            > = Rc::new(|_cx: &Context, _dst_cx: &Context| {
                Err(type_::any_t::annot(reason::locationless_reason(RExports)))
            });

            let file = File(Rc::new(FileInner {
                dependencies: RefCell::new(builtins.module_refs.map(|mref| {
                    let resolved = map_module_ref(mref, &dependencies_map);
                    (mref.dupe(), resolved)
                })),
                exports,
                local_defs: RefCell::new(
                    builtins
                        .local_defs
                        .map(|def| local_def_impl(&aloc, &self_ref, def)),
                ),
                remote_refs: RefCell::new(
                    builtins
                        .remote_refs
                        .map(|rref| remote_ref_fn(&self_ref, rref)),
                ),
                pattern_defs: RefCell::new(
                    builtins
                        .pattern_defs
                        .map(|def| pattern_def_fn(&self_ref, def)),
                ),
                patterns: RefCell::new(builtins.patterns.map(|p| pattern_fn(&self_ref, p))),
            }));

            let result = (file, dependencies_map);
            let _ = cell_ref.set(result.clone());
            result
        })
            as Box<dyn FnOnce(&Context<'cx>) -> FileAndDepMap<'cx> + 'cx>))
    };
    let builtin_values: BTreeMap<FlowSmolStr, flow_typing_builtins::LazyVal<'cx, Context<'cx>>> =
        builtins
            .global_values
            .iter()
            .map(|(name, i)| {
                let inner_lazy = local_def_impl(
                    &aloc,
                    &file_and_dependency_map_rec,
                    builtins.local_defs.get(*i),
                );
                let lazy_val: flow_typing_builtins::LazyVal<'cx, Context<'cx>> =
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        //   (fun (loc, _, (lazy t), _) -> (loc, t))
                        let (loc, _, general, _) = inner_lazy.get_forced(cx);
                        (loc.dupe(), general.get_forced(cx).dupe())
                    })
                        as Box<dyn FnOnce(&Context<'cx>) -> (ALoc, Type) + 'cx>));
                (name.dupe(), lazy_val)
            })
            .collect();

    let builtin_types: BTreeMap<FlowSmolStr, flow_typing_builtins::LazyVal<'cx, Context<'cx>>> =
        builtins
            .global_types
            .iter()
            .map(|(name, i)| {
                let inner_lazy = local_def_impl(
                    &aloc,
                    &file_and_dependency_map_rec,
                    builtins.local_defs.get(*i),
                );
                let lazy_val: flow_typing_builtins::LazyVal<'cx, Context<'cx>> =
                    Rc::new(flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        //   (fun (loc, _, (lazy t), _) -> (loc, t))
                        let (loc, _, general, _) = inner_lazy.get_forced(cx);
                        (loc.dupe(), general.get_forced(cx).dupe())
                    })
                        as Box<dyn FnOnce(&Context<'cx>) -> (ALoc, Type) + 'cx>));
                (name.dupe(), lazy_val)
            })
            .collect();

    let builtin_modules: BTreeMap<
        FlowSmolStr,
        flow_typing_builtins::LazyModule<'cx, Context<'cx>>,
    > = builtins
        .global_modules
        .keys()
        .map(|name| {
            let (_, dep_map) = file_and_dependency_map_rec.get_forced(cx);
            let module = dep_map
                .get(name)
                .expect("module not found in dep_map")
                .dupe();
            (name.dupe(), module)
        })
        .collect();

    (builtin_values, builtin_types, builtin_modules)
}
