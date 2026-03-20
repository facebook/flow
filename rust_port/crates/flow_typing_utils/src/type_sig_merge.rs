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
use once_cell::unsync::Lazy;
use vec1::Vec1;

use crate::annotation_inference;

pub enum Exports {
    CJSExports {
        type_exports:
            BTreeMap<FlowSmolStr, Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>>>,
        exports: Option<Rc<Lazy<(Option<ALoc>, Type), Box<dyn FnOnce() -> (Option<ALoc>, Type)>>>>,
        type_stars: Vec<(ALoc, Index<FlowImportSpecifier>)>,
        strict: bool,
        platform_availability_set: Option<flow_common::platform_set::PlatformSet>,
    },
    ESExports {
        type_exports:
            BTreeMap<FlowSmolStr, Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>>>,
        exports: BTreeMap<FlowSmolStr, Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>>>,
        type_stars: Vec<(ALoc, Index<FlowImportSpecifier>)>,
        stars: Vec<(ALoc, Index<FlowImportSpecifier>)>,
        strict: bool,
        platform_availability_set: Option<flow_common::platform_set::PlatformSet>,
    },
}

#[derive(Clone, Dupe)]
pub struct File(Rc<FileInner>);

pub struct FileInner {
    pub cx: Context,
    dependencies: Table<(
        Userland,
        Rc<Lazy<ResolvedRequire, Box<dyn FnOnce() -> ResolvedRequire>>>,
    )>,
    pub exports: Rc<dyn Fn() -> Result<ModuleType, Type>>,
    local_defs: Table<
        Rc<
            Lazy<
                (
                    ALoc,
                    FlowSmolStr,
                    // general
                    Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                    // const
                    Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                ),
                Box<
                    dyn FnOnce() -> (
                        ALoc,
                        FlowSmolStr,
                        Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                        Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                    ),
                >,
            >,
        >,
    >,
    remote_refs:
        Table<Rc<Lazy<(ALoc, FlowSmolStr, Type), Box<dyn FnOnce() -> (ALoc, FlowSmolStr, Type)>>>>,
    patterns: Table<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>,
    pattern_defs: Table<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>,
}

impl std::ops::Deref for File {
    type Target = FileInner;
    fn deref(&self) -> &FileInner {
        &self.0
    }
}

impl File {
    pub fn new(
        cx: Context,
        dependencies: Table<(
            Userland,
            Rc<Lazy<ResolvedRequire, Box<dyn FnOnce() -> ResolvedRequire>>>,
        )>,
        exports: Rc<dyn Fn() -> Result<ModuleType, Type>>,
        local_defs: Table<
            Rc<
                Lazy<
                    (
                        ALoc,
                        FlowSmolStr,
                        Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                        Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                    ),
                    Box<
                        dyn FnOnce() -> (
                            ALoc,
                            FlowSmolStr,
                            Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                            Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
                        ),
                    >,
                >,
            >,
        >,
        remote_refs: Table<
            Rc<Lazy<(ALoc, FlowSmolStr, Type), Box<dyn FnOnce() -> (ALoc, FlowSmolStr, Type)>>>,
        >,
        pattern_defs: Table<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>,
        patterns: Table<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>,
    ) -> Self {
        File(Rc::new(FileInner {
            cx,
            dependencies,
            exports,
            local_defs,
            remote_refs,
            pattern_defs,
            patterns,
        }))
    }

    /// Access the Context owned by this File.
    /// Used to break Rc cycles when the File is evicted from cache or cleaned up.
    pub fn cx(&self) -> &Context {
        &self.0.cx
    }

    /// Create a Weak reference to the inner FileInner.
    /// Used in file_cell to break self-referential Rc cycles:
    /// File → Lazy closures → file_cell → File
    pub fn downgrade(&self) -> Weak<FileInner> {
        Rc::downgrade(&self.0)
    }

    /// Reconstruct a File from a Weak reference stored in file_cell.
    /// The Weak upgrade is safe because this is only called from Lazy closures
    /// stored within the File itself — the File must be alive for the closure
    /// to be called.
    pub fn from_weak(weak: &Weak<FileInner>) -> Self {
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
        Def::TypeAlias { id_loc, name, .. } | Def::OpaqueType { id_loc, name, .. } => {
            type_::desc_format::type_reason(Name::new(name.dupe()), id_loc.dupe())
        }
        Def::Interface { id_loc, name, .. }
        | Def::ClassBinding { id_loc, name, .. }
        | Def::DeclareClassBinding { id_loc, name, .. }
        | Def::RecordBinding { id_loc, name, .. }
        | Def::DisabledRecordBinding { id_loc, name, .. } => {
            type_::desc_format::instance_reason(Name::new(name.dupe()), id_loc.dupe())
        }
        Def::FunBinding {
            fn_loc,
            async_,
            generator,
            ..
        } => reason::func_reason(*async_, *generator, fn_loc.dupe()),
        Def::DeclareFun { id_loc, .. } => reason::mk_reason(RFunctionType, id_loc.dupe()),
        Def::ComponentBinding { fn_loc, name, .. } => {
            reason::mk_reason(RComponent(Name::new(name.dupe())), fn_loc.dupe())
        }
        Def::DisabledComponentBinding { id_loc, name } => {
            reason::mk_reason(RIdentifier(Name::new(name.dupe())), id_loc.dupe())
        }
        Def::Variable { id_loc, name, .. } | Def::Parameter { id_loc, name, .. } => {
            reason::mk_reason(RIdentifier(Name::new(name.dupe())), id_loc.dupe())
        }
        Def::DisabledEnumBinding { id_loc, name, .. } | Def::EnumBinding { id_loc, name, .. } => {
            reason::mk_reason(
                REnum {
                    name: Some(name.dupe()),
                },
                id_loc.dupe(),
            )
        }
        Def::NamespaceBinding { id_loc, name, .. } => {
            reason::mk_reason(RNamespace(name.dupe()), id_loc.dupe())
        }
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

fn eval_id_of_aloc(file: &File, loc: ALoc) -> type_::eval::Id {
    type_::eval::Id::of_aloc_id(true, file.cx.make_aloc_id(&loc))
}

fn specialize(file: &File, reason_op: Reason, t: Type) -> Type {
    let reason = type_util::reason_of_t(&t).dupe();
    annotation_inference::specialize(&file.cx, t, type_::unknown_use(), reason_op, reason, None)
}

/// Repositioning the underlying type does not seem to have any perceptible impact
/// when dealing with annotations. Instead of invoking the convoluted Flow_js.reposition
/// implementation here, we just return the type intact. What does have an effect is the
/// lazy tvar indirection, which updates the reason on the new OpenT.
fn reposition_sig_tvar(cx: &Context, loc: ALoc, t: Type) -> Type {
    let reason = type_util::reason_of_t(&t).dupe().reposition(loc);
    annotation_inference::mk_sig_tvar(cx, reason, Rc::new(Lazy::new(Box::new(move || t))))
}

fn eval_arith(
    file: &File,
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
    annotation_inference::arith(&file.cx, reason, lhs_t, rhs_t, kind)
}

fn eval_unary(
    file: &File,
    loc: ALoc,
    t: Type,
    op: flow_parser::ast::expression::UnaryOperator,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_parser::ast::expression::UnaryOperator;
    match op {
        UnaryOperator::Minus => {
            let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
            annotation_inference::unary_arith(&file.cx, reason, t, type_::UnaryArithKind::Minus)
        }
        UnaryOperator::Plus => {
            let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
            annotation_inference::unary_arith(&file.cx, reason, t, type_::UnaryArithKind::Plus)
        }
        UnaryOperator::BitNot => {
            let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
            annotation_inference::unary_arith(&file.cx, reason, t, type_::UnaryArithKind::BitNot)
        }
        UnaryOperator::Not => {
            let reason = reason::mk_reason(
                RUnaryOperator("not".into(), Arc::new(type_util::desc_of_t(&t).clone())),
                loc,
            );
            annotation_inference::unary_not(&file.cx, reason, t)
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

fn eval_update(file: &File, loc: ALoc, t: Type) -> Type {
    let reason = reason::mk_reason(type_util::desc_of_t(&t).clone(), loc);
    annotation_inference::unary_arith(&file.cx, reason, t, type_::UnaryArithKind::Update)
}

fn eval(file: &File, loc: ALoc, t: Type, op: Op<Type>) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    match op {
        Op::Arith(bin_op, rhs_t) => eval_arith(file, loc, t, rhs_t, bin_op),
        Op::Unary(unary_op) => eval_unary(file, loc, t, unary_op),
        Op::Update => eval_update(file, loc, t),
        Op::GetProp(name) => {
            let name = Name::new(name);
            let reason = reason::mk_reason(RProperty(Some(name.dupe())), loc);
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_prop(&file.cx, use_op, reason, None, name, t)
        }
        Op::GetElem(index) => {
            let reason = reason::mk_reason(RProperty(None), loc);
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_elem(&file.cx, use_op, reason, index, t)
        }
    }
}

fn async_void_return(file: &File, loc: ALoc) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    let reason = reason::mk_reason(RAsyncReturn, loc.dupe());
    let targs = vec![flow_typing_type::type_::void::at(loc)];
    flow_js_utils::lookup_builtin_typeapp(&file.cx, reason, "Promise", targs)
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

fn add_record_constructor(
    file: &File,
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
    let mut props = type_::properties::PropertiesMap::new();
    for (prop_name, prop) in own_props {
        let new_prop = match &**prop {
            type_::PropertyInner::Field {
                preferred_def_locs,
                key_loc,
                type_,
                polarity,
            } => {
                if defaulted_props.contains(prop_name) {
                    let r = type_util::reason_of_t(type_).dupe();
                    let optional_reason = r.dupe().update_desc_new(|desc| {
                        reason::VirtualReasonDesc::ROptional(Arc::new(desc))
                    });
                    type_::Property::new(type_::PropertyInner::Field {
                        preferred_def_locs: preferred_def_locs.clone(),
                        key_loc: key_loc.dupe(),
                        type_: Type::new(type_::TypeInner::OptionalT {
                            reason: optional_reason,
                            type_: type_.dupe(),
                            use_desc: false,
                        }),
                        polarity: polarity.clone(),
                    })
                } else {
                    prop.dupe()
                }
            }
            _ => prop.dupe(),
        };
        props.insert(Name::new(prop_name.dupe()), new_prop);
    }
    let param = obj_type::mk_with_proto(
        &file.cx,
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

fn add_name_field(reason: Reason, props: &mut type_::properties::PropertiesMap) {
    let name_key = Name::new(FlowSmolStr::new("name"));
    if !props.contains_key(&name_key) {
        let prop = type_::Property::new(type_::PropertyInner::Field {
            preferred_def_locs: None,
            key_loc: None,
            type_: type_::str_module_t::why(reason),
            polarity: Polarity::Neutral,
        });
        props.insert(name_key, prop);
    }
}

fn require(
    file: &File,
    loc: ALoc,
    index: Index<FlowImportSpecifier>,
    standard_cjs_esm_interop: bool,
) -> Type {
    let (mref, lazy_resolved) = file.dependencies.get(index);
    let resolved_require = Lazy::force(&**lazy_resolved).dupe();
    let reason = reason::mk_reason(reason::VirtualReasonDesc::RModule(mref.dupe()), loc.dupe());
    let symbol = Symbol::mk_module_symbol(mref.dupe().into_inner(), loc.dupe());
    annotation_inference::cjs_require(
        &file.cx,
        reason,
        symbol,
        false,
        standard_cjs_esm_interop,
        resolved_require,
    )
}

fn import(
    file: &File,
    reason: Reason,
    index: Index<FlowImportSpecifier>,
    kind: type_::ImportKind,
    remote: &FlowSmolStr,
    local: &FlowSmolStr,
) -> Type {
    let (mref, lazy_resolved) = file.dependencies.get(index);
    let resolved_require = Lazy::force(&**lazy_resolved).dupe();
    if remote.as_str() == "default" {
        annotation_inference::import_default(
            &file.cx,
            reason,
            kind,
            local,
            mref.dupe(),
            false,
            resolved_require,
        )
    } else {
        annotation_inference::import_named(
            &file.cx,
            reason,
            kind,
            remote,
            mref.dupe(),
            false,
            resolved_require,
        )
    }
}

fn import_ns(
    file: &File,
    reason: Reason,
    name: &FlowSmolStr,
    id_loc: ALoc,
    index: Index<FlowImportSpecifier>,
) -> Type {
    let (_, lazy_resolved) = file.dependencies.get(index);
    let resolved_require = Lazy::force(&**lazy_resolved).dupe();
    let namespace_symbol = Symbol::mk_module_symbol(
        Userland::from_smol_str(name.dupe()).into_inner(),
        id_loc.dupe(),
    );
    annotation_inference::import_ns(&file.cx, reason, namespace_symbol, false, resolved_require)
}

fn import_typeof_ns(
    file: &File,
    reason: Reason,
    name: &FlowSmolStr,
    id_loc: ALoc,
    index: Index<FlowImportSpecifier>,
) -> Type {
    let (_, lazy_resolved) = file.dependencies.get(index);
    let resolved_require = Lazy::force(&**lazy_resolved).dupe();
    let namespace_symbol = Symbol::mk_namespace_symbol(name.dupe(), id_loc.dupe());
    let ns_t = annotation_inference::import_ns(
        &file.cx,
        reason.dupe(),
        namespace_symbol,
        false,
        resolved_require,
    );
    annotation_inference::import_typeof(&file.cx, reason, "*", ns_t)
}

fn merge_enum(
    file: &File,
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
    let enum_id = file.cx.make_aloc_id(&id_loc);
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

pub fn merge_pattern(file: &File, pattern: &Pack::Pattern<ALoc>) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    match pattern {
        Pack::Pattern::PDef(i) => Lazy::force(file.pattern_defs.get(*i)).dupe(),
        Pack::Pattern::PropP { id_loc, name, def } => {
            let t = Lazy::force(file.patterns.get(*def)).dupe();
            let name = Name::new(name.dupe());
            let reason = reason::mk_reason(RProperty(Some(name.dupe())), id_loc.dupe());
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_prop(&file.cx, use_op, reason, None, name, t)
        }
        Pack::Pattern::ComputedP { elem, def } => {
            let elem = Lazy::force(file.pattern_defs.get(*elem)).dupe();
            let t = Lazy::force(file.patterns.get(*def)).dupe();
            let loc = type_util::loc_of_t(&elem);
            let reason = reason::mk_reason(RProperty(None), loc.dupe());
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::get_elem(&file.cx, use_op, reason, elem, t)
        }
        Pack::Pattern::UnsupportedLiteralP(loc) => {
            type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe())
        }
        Pack::Pattern::ObjRestP { loc, xs, def } => {
            let t = Lazy::force(file.patterns.get(*def)).dupe();
            let reason = reason::mk_reason(RObjectPatternRestProp, loc.dupe());
            annotation_inference::obj_rest(&file.cx, reason, xs.iter().duped().collect(), t)
        }
        Pack::Pattern::IndexP { loc, i, def } => {
            let t = Lazy::force(file.patterns.get(*def)).dupe();
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
            annotation_inference::get_elem(&file.cx, use_op, reason, i, t.dupe())
        }
        Pack::Pattern::ArrRestP { loc, i, def } => {
            let t = Lazy::force(file.patterns.get(*def)).dupe();
            let reason = reason::mk_reason(RArrayPatternRestProp, loc.dupe());
            // TODO: use_op
            let use_op = type_::unknown_use();
            annotation_inference::arr_rest(&file.cx, use_op, reason, *i as i32, t)
        }
    }
}

pub fn merge_remote_ref(file: &File, reason: Reason, remote_ref: &Pack::RemoteRef<ALoc>) -> Type {
    match remote_ref {
        Pack::RemoteRef::Import {
            id_loc: _,
            name,
            index,
            remote,
        } => import(
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
        } => import_ns(file, reason, name, id_loc.dupe(), *index),
        Pack::RemoteRef::ImportTypeofNs {
            id_loc,
            name,
            index,
        } => import_typeof_ns(file, reason, name, id_loc.dupe(), *index),
        Pack::RemoteRef::ImportTypeNs {
            id_loc,
            name,
            index,
        } => import_ns(file, reason, name, id_loc.dupe(), *index),
    }
}

fn merge_ref<R>(
    file: &File,
    f: impl FnOnce(Type, ALoc, ALoc, &FlowSmolStr) -> R,
    packed_ref: &Pack::PackedRef<ALoc>,
    const_decl: bool,
) -> R {
    use flow_common::reason::VirtualReasonDesc::*;

    match packed_ref {
        Pack::PackedRef::LocalRef { ref_loc, index } => {
            let entry = Lazy::force(file.local_defs.get(*index));
            let (def_loc, name, t_general, t_const) = entry;
            let t = Lazy::force(&*if const_decl {
                t_const.dupe()
            } else {
                t_general.dupe()
            })
            .dupe();
            let t = reposition_sig_tvar(&file.cx, ref_loc.dupe(), t);
            f(t, ref_loc.dupe(), def_loc.dupe(), name)
        }
        Pack::PackedRef::RemoteRef { ref_loc, index } => {
            let entry = Lazy::force(file.remote_refs.get(*index));
            let (def_loc, name, t) = entry;
            let t = reposition_sig_tvar(&file.cx, ref_loc.dupe(), t.dupe());
            f(t, ref_loc.dupe(), def_loc.dupe(), name)
        }
        Pack::PackedRef::BuiltinRef {
            ref_loc,
            type_ref,
            name,
        } => {
            let reason = reason::mk_reason(RIdentifier(Name::new(name.dupe())), ref_loc.dupe());
            let t = if *type_ref {
                flow_js_utils::lookup_builtin_type(&file.cx, name.as_str(), reason)
            } else {
                flow_js_utils::lookup_builtin_value(&file.cx, name.as_str(), reason)
            };
            let def_loc = type_util::reason_of_t(&t).def_loc().dupe();
            f(t, ref_loc.dupe(), def_loc, name)
        }
    }
}

fn merge_tyref<R>(
    file: &File,
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
                        &file.cx, use_op, id_reason, op_reason, name, t,
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

pub fn merge_type_export(
    file: &File,
    reason: Reason,
    type_export: &Pack::TypeExport<ALoc>,
) -> NamedSymbol {
    match type_export {
        Pack::TypeExport::ExportTypeRef(ref_) => {
            let reason = reason.dupe();
            merge_ref(
                file,
                |t, _ref_loc, def_loc, name| {
                    let type_ = annotation_inference::assert_export_is_type(
                        &file.cx,
                        reason,
                        name.as_str(),
                        t,
                    );
                    NamedSymbol::new(Some(def_loc), None, type_)
                },
                ref_,
                false,
            )
        }
        Pack::TypeExport::ExportTypeBinding(index) => {
            let entry = Lazy::force(file.local_defs.get(*index));
            let (loc, name, t_general, _t_const) = entry;
            let t = Lazy::force(&*t_general.clone()).dupe();
            let type_ =
                annotation_inference::assert_export_is_type(&file.cx, reason, name.as_str(), t);
            NamedSymbol::new(Some(loc.dupe()), None, type_)
        }
        Pack::TypeExport::ExportTypeFrom(index) => {
            let entry = Lazy::force(file.remote_refs.get(*index));
            let (loc, _name, type_) = entry;
            NamedSymbol::new(Some(loc.dupe()), None, type_.dupe())
        }
    }
}

fn mk_commonjs_module_t(
    cx: &Context,
    module_reason: Reason,
    module_is_strict: bool,
    module_available_platforms: Option<flow_common::platform_set::PlatformSet>,
    def_loc: Option<ALoc>,
    t: Type,
) -> Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>> {
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
    annotation_inference::lazy_cjs_extract_named_exports(cx, module_reason, local_module, t)
}

pub fn merge_exports(
    file: &File,
    reason: Reason,
    exports: Exports,
) -> Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>> {
    type FromNs = Option<Rc<dyn Fn() -> Result<ModuleType, Type>>>;

    fn merge_star(
        file: &File,
        (loc, index): &(ALoc, Index<FlowImportSpecifier>),
    ) -> (ALoc, FromNs) {
        let (_, lazy_resolved) = file.dependencies.get(index.clone());
        let resolved_require = Lazy::force(&**lazy_resolved).dupe();
        let f: FromNs = match resolved_require {
            ResolvedRequire::TypedModule(f) => Some(f),
            ResolvedRequire::UncheckedModule(_) => None,
            ResolvedRequire::MissingModule => None,
        };
        (loc.dupe(), f)
    }

    fn mk_es_module_t(
        file: &File,
        module_reason: Reason,
        module_is_strict: bool,
        module_available_platforms: Option<flow_common::platform_set::PlatformSet>,
    ) -> ModuleType {
        let module_export_types = type_::ExportTypes {
            value_exports_tmap: file.cx.make_export_map(type_::exports::T::new()),
            type_exports_tmap: file.cx.make_export_map(type_::exports::T::new()),
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

    fn copy_named_exports_star(
        file: &File,
        target_module_type: &ModuleType,
        (_, from_ns): &(ALoc, FromNs),
    ) {
        match from_ns {
            None => (),
            Some(f) => {
                annotation_inference::copy_named_exports(&file.cx, f(), target_module_type);
            }
        }
    }

    fn copy_type_exports_star(
        file: &File,
        reason: &Reason,
        target_module_type: &ModuleType,
        (loc, from_ns): &(ALoc, FromNs),
    ) {
        match from_ns {
            None => (),
            Some(f) => {
                let reason = reason.dupe().reposition(loc.dupe());
                annotation_inference::copy_type_exports(&file.cx, f(), reason, target_module_type);
            }
        }
    }

    fn copy_star_exports(
        file: &File,
        reason: &Reason,
        xs: &[(ALoc, FromNs)],
        ys: &[(ALoc, FromNs)],
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
                        copy_named_exports_star(file, target_module_type, x);
                    }
                    break;
                }
                ([], ys) => {
                    for y in ys {
                        copy_type_exports_star(file, reason, target_module_type, y);
                    }
                    break;
                }
                ([x, xs_ @ ..], [y, ys_ @ ..]) => {
                    if x.0.cmp(&y.0) == std::cmp::Ordering::Greater {
                        copy_named_exports_star(file, target_module_type, x);
                        xs = xs_;
                    } else {
                        copy_type_exports_star(file, reason, target_module_type, y);
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
                    let (def_loc_opt, t) = Lazy::force(&*lazy_val).clone();
                    (def_loc_opt, t)
                }
                None => (
                    None,
                    obj_type::mk_with_proto(
                        &file.cx,
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
                .map(|(key, lazy_val)| (Name::new(key.dupe()), Lazy::force(lazy_val).clone()))
                .collect();
            let type_stars: Vec<(ALoc, FromNs)> =
                type_stars.iter().map(|s| merge_star(file, s)).collect();
            let file2 = file.dupe();
            let reason2 = reason.dupe();
            Rc::new(Lazy::new(Box::new(move || {
                let lazy_module = mk_commonjs_module_t(
                    &file2.cx,
                    reason2.dupe(),
                    strict,
                    platform_availability_set,
                    def_loc_opt,
                    exports_t,
                );
                let module_type = Lazy::force(&*lazy_module).dupe();
                flow_js_utils::export_named_t_kit::mod_module_t(
                    &file2.cx,
                    type_::exports::T::new(),
                    type_exports_map,
                    type_::ExportKind::DirectExport,
                    &module_type,
                );
                copy_star_exports(&file2, &reason2, &[], &type_stars, &module_type);
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
                .map(|(key, lazy_val)| (Name::new(key.dupe()), Lazy::force(lazy_val).clone()))
                .collect();
            // let type_exports = SMap.map Lazy.force type_exports |> NameUtils.namemap_of_smap in
            let type_exports_map: type_::exports::T = type_exports
                .iter()
                .map(|(key, lazy_val)| (Name::new(key.dupe()), Lazy::force(lazy_val).clone()))
                .collect();
            let stars: Vec<(ALoc, FromNs)> = stars.iter().map(|s| merge_star(file, s)).collect();
            let type_stars: Vec<(ALoc, FromNs)> =
                type_stars.iter().map(|s| merge_star(file, s)).collect();
            let file2 = file.dupe();
            let reason2 = reason.dupe();
            Rc::new(Lazy::new(Box::new(move || {
                let module_type =
                    mk_es_module_t(&file2, reason2.dupe(), strict, platform_availability_set);
                flow_js_utils::export_named_t_kit::mod_module_t(
                    &file2.cx,
                    exports_map,
                    type_exports_map,
                    type_::ExportKind::DirectExport,
                    &module_type,
                );
                copy_star_exports(&file2, &reason2, &stars, &type_stars, &module_type);
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

fn merge_impl(
    env: &MergeEnv,
    file: &File,
    packed: &Pack::Packed<ALoc>,
    as_const: bool,
    const_decl: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    match packed {
        Pack::Packed::Annot(t) => merge_annot(env, file, t),
        Pack::Packed::Value(t) => merge_value(env, file, t, as_const, const_decl),
        // Let's have the property retain the precise type in
        // export const FOO = "foo";
        // export const OBJ = { FOO } as const;
        Pack::Packed::Ref(ref_) => merge_ref(
            file,
            |t, _ref_loc, _def_loc, _name| t,
            ref_,
            as_const || const_decl,
        ),
        Pack::Packed::TyRef(name) => {
            let in_renders_arg = env.in_renders_arg;
            merge_tyref(
                file,
                |t, ref_loc, names| {
                    let (name, _) = (names.first().duped().unwrap_or_default(), &names[1..]);
                    let reason = reason::mk_annot_reason(RType(Name::new(name)), ref_loc);
                    let type_t_kind = if in_renders_arg {
                        type_::TypeTKind::RenderTypeKind
                    } else {
                        type_::TypeTKind::TypeAliasKind
                    };
                    annotation_inference::mk_type_reference(&file.cx, type_t_kind, reason, t)
                },
                name,
            )
        }
        Pack::Packed::TyRefApp { loc, name, targs } => {
            let targs: Vec<Type> = targs
                .iter()
                .map(|t| merge_impl(env, file, t, false, false))
                .collect();
            let loc = loc.dupe();
            merge_tyref(
                file,
                move |t, _ref_loc, _names| type_util::typeapp_annot(false, false, loc, t, targs),
                name,
            )
        }
        Pack::Packed::AsyncVoidReturn(loc) => async_void_return(file, loc.dupe()),
        Pack::Packed::Pattern(i) => Lazy::force(file.patterns.get(*i)).dupe(),
        Pack::Packed::Err(loc) => type_::any_t::at(type_::AnySource::AnyError(None), loc.dupe()),
        Pack::Packed::Eval(loc, t, op) => {
            let (eval_as_const, eval_const_decl) = match op {
                Op::Unary(flow_parser::ast::expression::UnaryOperator::Minus)
                | Op::Unary(flow_parser::ast::expression::UnaryOperator::Not) => {
                    (as_const, const_decl)
                }
                _ => (false, false),
            };
            let merged_t = merge_impl(env, file, t, eval_as_const, eval_const_decl);
            let merged_op = merge_op(env, file, op);
            eval(file, loc.dupe(), merged_t, merged_op)
        }
        Pack::Packed::Require { loc, index } => require(file, loc.dupe(), *index, false),
        Pack::Packed::ImportDynamic { loc, index } => {
            let (mref, _) = file.dependencies.get(*index);
            let ns_reason = reason::mk_reason(RModule(mref.dupe()), loc.dupe());
            let name = mref.dupe().into_inner();
            let ns_t = import_ns(file, ns_reason, &name, loc.dupe(), index.clone());
            let reason = reason::mk_annot_reason(RAsyncImport, loc.dupe());
            flow_js_utils::lookup_builtin_typeapp(&file.cx, reason, "Promise", vec![ns_t])
        }
        Pack::Packed::ModuleRef { loc, index } => {
            let t = require(file, loc.dupe(), *index, true);
            let reason = reason::mk_reason(RModuleReference, loc.dupe());
            flow_js_utils::lookup_builtin_typeapp(&file.cx, reason, "$Flow$ModuleRef", vec![t])
        }
        Pack::Packed::ImportTypeAnnot { loc, index } => {
            let (mref, _) = file.dependencies.get(*index);
            let ns_reason = reason::mk_reason(RModule(mref.dupe()), loc.dupe());
            let name = mref.dupe().into_inner();
            import_ns(file, ns_reason, &name, loc.dupe(), *index)
        }
    }
}

fn merge_annot(env: &MergeEnv, file: &File, annot: &Pack::PackedAnnot<ALoc>) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_type_sig::type_sig::Annot;
    match annot {
        Annot::Any(loc) => type_::any_t::at(type_::AnySource::AnnotatedAny, loc.dupe()),
        Annot::Mixed(loc) => type_::mixed_t::at(loc.dupe()),
        Annot::Empty(loc) => type_::empty_t::at(loc.dupe()),
        Annot::Void(loc) => type_::void::at(loc.dupe()),
        Annot::Null(loc) => type_::null::at(loc.dupe()),
        Annot::Symbol(loc) => type_::symbol_t::at(loc.dupe()),
        Annot::UniqueSymbol(loc) => {
            type_::unique_symbol_t::at(file.cx.make_aloc_id(loc), loc.dupe())
        }
        Annot::Number(loc) => type_::num_module_t::at(loc.dupe()),
        Annot::BigInt(loc) => type_::bigint_module_t::at(loc.dupe()),
        Annot::String(loc) => type_::str_module_t::at(loc.dupe()),
        Annot::Boolean(loc) => type_::bool_module_t::at(loc.dupe()),
        Annot::Exists(loc) => type_::any_t::at(type_::AnySource::AnnotatedAny, loc.dupe()),
        Annot::Optional(t) => {
            let t = merge_impl(env, file, t, false, false);
            type_util::optional(t, None, false)
        }
        Annot::Maybe(loc, t) => {
            let t = merge_impl(env, file, t, false, false);
            let desc = type_util::desc_of_t(&t).clone();
            let reason = reason::mk_annot_reason(RMaybe(Arc::new(desc)), loc.dupe());
            Type::new(type_::TypeInner::MaybeT(reason, t))
        }
        Annot::Union { loc, t0, t1, ts } => {
            let reason = reason::mk_annot_reason(RUnionType, loc.dupe());
            let t0 = merge_impl(env, file, t0, false, false);
            let t1 = merge_impl(env, file, t1, false, false);
            let ts: Vec<Type> = ts
                .iter()
                .map(|t| merge_impl(env, file, t, false, false))
                .collect();
            let source_aloc = Some(file.cx.make_aloc_id(loc));
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
        Annot::Intersection { loc, t0, t1, ts } => {
            let reason = reason::mk_annot_reason(RIntersectionType, loc.dupe());
            let t0 = merge_impl(env, file, t0, false, false);
            let t1 = merge_impl(env, file, t1, false, false);
            let ts: Vec<Type> = ts
                .iter()
                .map(|t| merge_impl(env, file, t, false, false))
                .collect();
            Type::new(type_::TypeInner::IntersectionT(
                reason,
                type_::inter_rep::make(t0, t1, ts.into()),
            ))
        }
        Annot::Tuple {
            loc,
            elems,
            inexact,
        } => {
            let reason = reason::mk_annot_reason(RTupleType, loc.dupe());
            let unresolved: Vec<type_::UnresolvedParam> = elems
                .iter()
                .map(|elem| match elem {
                    type_sig::TupleElement::TupleElement {
                        loc,
                        name,
                        t,
                        polarity,
                        optional,
                    } => {
                        let reason =
                            reason::mk_reason(RTupleElement { name: name.dupe() }, loc.dupe());
                        let t = merge_impl(env, file, t, false, false);
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
                        let t = merge_impl(env, file, t, false, false);
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
            let id = eval_id_of_aloc(file, loc.dupe());
            flow_js_utils::mk_tuple_type(
                &file.cx,
                id,
                mk_type_destructor,
                *inexact,
                reason,
                unresolved,
            )
            .unwrap()
        }
        Annot::Array(loc, t) => {
            let reason = reason::mk_annot_reason(RArrayType, loc.dupe());
            let elem_t = merge_impl(env, file, t, false, false);
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ArrayAT {
                    elem_t,
                    tuple_view: None,
                    react_dro: None,
                }))),
            ))
        }
        Annot::ReadOnlyArray(loc, t) => {
            let reason = reason::mk_annot_reason(RROArrayType, loc.dupe());
            let t = merge_impl(env, file, t, false, false);
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::ArrT(Rc::new(type_::ArrType::ROArrayAT(
                    t, None,
                )))),
            ))
        }
        Annot::SingletonString(loc, str) => {
            let reason = reason::mk_annot_reason(RStringLit(Name::new(str.dupe())), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::SingletonStrT {
                    from_annot: true,
                    value: Name::new(str.dupe()),
                }),
            ))
        }
        Annot::SingletonNumber(loc, num, raw) => {
            let reason = reason::mk_annot_reason(RNumberLit(raw.dupe()), loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::SingletonNumT {
                    from_annot: true,
                    value: type_::NumberLiteral(*num, raw.dupe()),
                }),
            ))
        }
        Annot::SingletonBigInt(loc, bigint, raw) => {
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
        Annot::StringPrefix {
            loc,
            prefix,
            remainder,
        } => {
            let reason = reason::mk_reason(
                RStringPrefix {
                    prefix: prefix.dupe(),
                },
                loc.dupe(),
            );
            let remainder = remainder
                .as_ref()
                .map(|t| merge_impl(env, file, t, false, false));
            Type::new(type_::TypeInner::StrUtilT {
                reason,
                op: type_::StrUtilOp::StrPrefix(prefix.dupe()),
                remainder,
            })
        }
        Annot::StringSuffix {
            loc,
            suffix,
            remainder,
        } => {
            let reason = reason::mk_reason(
                RStringSuffix {
                    suffix: suffix.dupe(),
                },
                loc.dupe(),
            );
            let remainder = remainder
                .as_ref()
                .map(|t| merge_impl(env, file, t, false, false));
            Type::new(type_::TypeInner::StrUtilT {
                reason,
                op: type_::StrUtilOp::StrSuffix(suffix.dupe()),
                remainder,
            })
        }
        Annot::Typeof {
            loc,
            qname,
            t,
            targs,
        } => {
            let qname_str: FlowSmolStr = qname.join(".").into();
            let reason = reason::mk_reason(RTypeof(qname_str), loc.dupe());
            let t = merge_impl(env, file, t, false, true);
            let targs = targs.as_ref().map(|ts| {
                ts.iter()
                    .map(|t| merge_impl(env, file, t, false, false))
                    .collect()
            });
            type_util::typeof_annotation(reason, t, targs)
        }
        Annot::Bound { ref_loc, name } => {
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
            merge_impl(&new_env, file, t, false, false)
        }
        Annot::PropertyType { loc, obj, prop } => {
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("$PropertyType"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let obj = merge_impl(env, file, obj, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::ElementType { loc, obj, elem } => {
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("$ElementType"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let obj = merge_impl(env, file, obj, false, false);
            let index_type = merge_impl(env, file, elem, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::EnumValue(loc, t) => {
            let reason = reason::mk_annot_reason(REnum { name: None }, loc.dupe());
            let representation_t = merge_impl(env, file, t, false, false);
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::EnumValueT(Rc::new(type_::EnumInfo::new(
                    type_::EnumInfoInner::AbstractEnum { representation_t },
                )))),
            ))
        }
        Annot::Enum(loc, t) => {
            let reason = reason::mk_annot_reason(REnum { name: None }, loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, file, t, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::OptionalIndexedAccessNonMaybeType { loc, obj, index } => {
            let reason = reason::mk_reason(RIndexedAccess { optional: true }, loc.dupe());
            let object_type = merge_impl(env, file, obj, false, false);
            let index_type = merge_impl(env, file, index, false, false);
            let object_reason = type_util::reason_of_t(&object_type).dupe();
            let index_reason = type_util::reason_of_t(&index_type).dupe();
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::IndexedTypeAccess {
                object: object_reason,
                index: index_reason,
            }));
            let id = eval_id_of_aloc(file, loc.dupe());
            // let index = match index with ...
            let oia_index = match index {
                Pack::Packed::Annot(annot) => match annot.as_ref() {
                    Annot::SingletonString(_, str) => {
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
        Annot::OptionalIndexedAccessResultType {
            loc,
            non_maybe_result,
            void_loc,
        } => {
            let reason = reason::mk_reason(RIndexedAccess { optional: true }, loc.dupe());
            let void_reason = reason::mk_reason(RVoid, void_loc.dupe());
            let non_maybe_result_type = merge_impl(env, file, non_maybe_result, false, false);
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
        Annot::NonMaybeType(loc, t) => {
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("$NonMaybeType"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, file, t, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::Omit(loc, t1, t2) => {
            let reason = reason::mk_reason(RType(Name::new(FlowSmolStr::new("Omit"))), loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t1 = merge_impl(env, file, t1, false, false);
            let t2 = merge_impl(env, file, t2, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::ReadOnly(loc, t) => {
            let reason = reason::mk_reason(RReadOnlyType, loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, file, t, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::Partial(loc, t) => {
            let t = merge_impl(env, file, t, false, false);
            let reason = reason::mk_reason(
                RPartialOf(Arc::new(type_util::desc_of_t(&t).clone())),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::Required(loc, t) => {
            let t = merge_impl(env, file, t, false, false);
            let reason = reason::mk_reason(
                RRequiredOf(Arc::new(type_util::desc_of_t(&t).clone())),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::Keys(loc, t) => {
            let reason = reason::mk_reason(RKeySet, loc.dupe());
            let t = merge_impl(env, file, t, false, false);
            Type::new(type_::TypeInner::KeysT(reason, t))
        }
        Annot::Values(loc, t) => {
            let reason =
                reason::mk_reason(RType(Name::new(FlowSmolStr::new("$Values"))), loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, file, t, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::Exact(loc, t) => {
            let t = merge_impl(env, file, t, false, false);
            let desc = type_util::desc_of_t(&t).clone();
            let reason = reason::mk_annot_reason(RExactType(Arc::new(desc)), loc.dupe());
            let t = type_util::push_type_alias_reason(&reason, t);
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::ExportsT(loc, mref) => {
            let reason = reason::mk_annot_reason(
                reason::VirtualReasonDesc::RModule(mref.dupe()),
                loc.dupe(),
            );
            let symbol = Symbol::mk_module_symbol(mref.dupe().into_inner(), loc.dupe());
            let cx = &file.cx;
            let f: ResolvedRequire = match cx.builtin_module_opt(mref) {
                Some((_reason, module_type)) => {
                    let module_type = module_type.dupe();
                    ResolvedRequire::TypedModule(Rc::new(move || Ok(module_type.dupe())))
                }
                None => {
                    let err_t = flow_js_utils::lookup_builtin_module_error(
                        cx,
                        &mref.dupe().into_inner(),
                        loc.dupe(),
                    )
                    .unwrap();
                    ResolvedRequire::TypedModule(Rc::new(move || Err(err_t.dupe())))
                }
            };
            annotation_inference::cjs_require(cx, reason, symbol, false, false, f)
        }
        Annot::Conditional {
            loc,
            distributive_tparam,
            infer_tparams,
            check_type,
            extends_type,
            true_type,
            false_type,
        } => {
            let reason = reason::mk_reason(RConditionalType, loc.dupe());
            let id = eval_id_of_aloc(file, loc.dupe());
            let convert = |distributive_tparam_name: Option<SubstName>, env: &MergeEnv| {
                let check_t = merge_impl(env, file, check_type, false, false);
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
                                merge_tparam(&mut env.dupe(), file, tp, true);
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
                let extends_t = merge_impl(&extends_env, file, extends_type, false, false);
                let true_env = MergeEnv {
                    tps: tps_for_true_type,
                    ..env.dupe()
                };
                let true_t = merge_impl(&true_env, file, true_type, false, false);
                let false_t = merge_impl(env, file, false_type, false, false);
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
        Annot::ObjKeyMirror { loc, obj } => {
            let reason = reason::mk_reason(RObjectKeyMirror, loc.dupe());
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let obj = merge_impl(env, file, obj, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::ClassT(loc, t) => {
            let t = merge_impl(env, file, t, false, false);
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
        Annot::ReactElementConfig(loc, t) => {
            let reason = reason::mk_reason(
                RType(Name::new(FlowSmolStr::new("React$ElementConfig"))),
                loc.dupe(),
            );
            let use_op = type_::UseOp::Op(Arc::new(type_::RootUseOp::TypeApplication {
                type_: reason.dupe(),
            }));
            let t = merge_impl(env, file, t, false, false);
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::Renders { loc, arg, variant } => {
            let mut renders_env = env.dupe();
            renders_env.in_renders_arg = true;
            let t = merge_impl(&renders_env, file, arg, false, false);
            let reason = reason::mk_annot_reason(
                RRenderType(Arc::new(type_util::reason_of_t(&t).desc(false).clone())),
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
                    annotation_inference::mk_non_generic_render_type(
                        &file.cx,
                        reason,
                        renders_variant,
                        t,
                    )
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
        Annot::FunAnnot(loc, def) => {
            let reason = reason::mk_annot_reason(RFunctionType, loc.dupe());
            let statics = merge_fun_statics(env, file, reason.dupe(), &BTreeMap::new());
            merge_fun(env, file, reason, def, statics, false, false)
        }
        Annot::ComponentAnnot(loc, def) => {
            let reason = reason::mk_annot_reason(RComponentType, loc.dupe());
            merge_component(env, file, reason, true, def, None)
        }
        Annot::ObjAnnot {
            loc,
            obj_kind,
            props,
            proto,
        } => {
            let reason = reason::mk_annot_reason(RObjectType, loc.dupe());
            let obj_kind = match obj_kind {
                type_sig::ObjKind::ExactObj => type_::ObjKind::Exact,
                type_sig::ObjKind::InexactObj => type_::ObjKind::Inexact,
                type_sig::ObjKind::IndexedObj(dict) => {
                    type_::ObjKind::Indexed(merge_dict(env, file, dict, false))
                }
            };
            let props_map: type_::properties::PropertiesMap = props
                .iter()
                .map(|(key, prop)| {
                    let p = merge_obj_annot_prop(env, file, prop);
                    (Name::new(key.dupe()), p)
                })
                .collect();
            let mk_object = |call: Option<Type>, proto: Type| {
                let id = type_::properties::Id::of_aloc_id(true, file.cx.make_aloc_id(loc));
                let flags = type_::Flags {
                    obj_kind: obj_kind.clone(),
                    react_dro: None,
                };
                let call = call.map(|t| file.cx.make_call_prop(t));
                file.cx.add_property_map(id.dupe(), props_map.dupe());
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
                        &file.cx,
                        proto_reason.dupe(),
                        merge_impl(env, file, t, false, false),
                    );
                    let proto = type_util::typeof_annotation(proto_reason, proto, None);
                    mk_object(None, proto)
                }
                ObjAnnotProto::ObjAnnotCallable { ts } => {
                    let proto = Type::new(type_::TypeInner::FunProtoT(reason.dupe()));
                    let ts_vec: Vec<Type> = ts
                        .iter()
                        .map(|t| {
                            let t = merge_impl(env, file, t, false, false);
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
        Annot::ObjSpreadAnnot { loc, exact, elems } => {
            let reason = reason::mk_annot_reason(RObjectType, loc.dupe());
            let target = type_::object::spread::Target::Annot { make_exact: *exact };
            let merge_slice = |dict: &Option<ObjAnnotDict<Pack::Packed<ALoc>>>,
                               props: &BTreeMap<
                FlowSmolStr,
                ObjAnnotProp<ALoc, Pack::Packed<ALoc>>,
            >| {
                let dict = dict.as_ref().map(|d| merge_dict(env, file, d, false));
                let mut prop_map = BTreeMap::new();
                for (key, prop) in props {
                    let p = merge_obj_annot_prop(env, file, prop);
                    prop_map.insert(Name::new(key.dupe()), p);
                }
                type_::object::spread::OperandSlice::new(type_::object::spread::OperandSliceInner {
                    reason: reason.dupe(),
                    prop_map: prop_map.into_iter().collect(),
                    generics: flow_typing_generics::spread_empty(),
                    dict,
                    reachable_targs: vec![].into(),
                })
            };
            let merge_elem = |elem: &ObjSpreadAnnotElem<ALoc, Pack::Packed<ALoc>>| match elem {
                ObjSpreadAnnotElem::ObjSpreadAnnotElem(t) => {
                    type_::object::spread::Operand::Type(merge_impl(env, file, t, false, false))
                }
                ObjSpreadAnnotElem::ObjSpreadAnnotSlice { dict, props } => {
                    type_::object::spread::Operand::Slice(merge_slice(dict, props))
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
            let id = eval_id_of_aloc(file, loc.dupe());
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
        Annot::InlineInterface(loc, def) => {
            let reason = reason::mk_annot_reason(RInterfaceType, loc.dupe());
            let id = file.cx.make_aloc_id(loc);
            merge_interface(env, file, reason, None, id, def, true, vec![])
        }
        Annot::MappedTypeAnnot {
            loc,
            source_type,
            property_type,
            key_tparam,
            variance,
            variance_op,
            optional,
            inline_keyof,
        } => {
            let source_type = merge_impl(env, file, source_type, false, false);
            let mut env = env.dupe();
            let (tp, _) = merge_tparam(&mut env, file, key_tparam, false);
            let property_type = {
                let prop_type = merge_impl(&env, file, property_type, false, false);
                let prop_reason = type_util::reason_of_t(&prop_type).dupe();
                let id = file.cx.make_source_poly_id(true, loc);
                Type::new(type_::TypeInner::DefT(
                    prop_reason,
                    type_::DefT::new(type_::DefTInner::PolyT {
                        tparams_loc: loc.dupe(),
                        tparams: vec![tp].into(),
                        t_out: prop_type,
                        id,
                    }),
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
            let id = eval_id_of_aloc(file, loc.dupe());
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
                        let (_, constraints) = file.cx.find_constraints(tvar.id() as i32);
                        match constraints {
                            type_::constraint::Constraints::FullyResolved(s) => {
                                let forced = file.cx.force_fully_resolved_tvar(&s);
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
                    let (_, constraints) = file.cx.find_constraints(tvar.id() as i32);
                    match constraints {
                        type_::constraint::Constraints::FullyResolved(s) => {
                            let forced = file.cx.force_fully_resolved_tvar(&s);
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

fn merge_value(
    env: &MergeEnv,
    file: &File,
    value: &Pack::PackedValue<ALoc>,
    as_const: bool,
    const_decl: bool,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    use flow_type_sig::type_sig::Value;
    match value {
        Value::ClassExpr(loc, def) => {
            let name: FlowSmolStr = "<<anonymous class>>".into();
            let reason = type_::desc_format::instance_reason(Name::new(name), loc.dupe());
            let id = file.cx.make_aloc_id(loc);
            merge_class(env, file, reason, None, id, def)
        }
        Value::FunExpr {
            loc,
            async_,
            generator,
            def,
            statics,
        } => {
            let reason = reason::func_reason(*async_, *generator, loc.dupe());
            let statics_t = merge_fun_statics(env, file, reason.dupe(), statics);
            merge_fun(env, file, reason, def, statics_t, false, false)
        }
        Value::StringVal(loc) => {
            let reason = reason::mk_reason(RString, loc.dupe());
            Type::new(type_::TypeInner::DefT(
                reason,
                type_::DefT::new(type_::DefTInner::StrGeneralT(type_::Literal::AnyLiteral)),
            ))
        }
        Value::StringLit(loc, lit) => {
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
        Value::NumberLit(loc, num, raw) => {
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
        Value::BigIntLit(loc, bigint, raw) => {
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
        Value::DeclareModuleImplicitlyExportedObject {
            loc,
            module_name,
            props,
        } => merge_declare_module_implicitly_exported_object(
            env,
            file,
            loc.dupe(),
            module_name,
            props,
        ),
        Value::ObjLit {
            loc,
            frozen,
            proto,
            props,
        } => merge_object_lit(
            env,
            file,
            loc.dupe(),
            *frozen,
            proto,
            props,
            false,
            as_const,
        ),
        Value::ObjSpreadLit {
            loc,
            frozen,
            proto,
            elems,
        } => merge_obj_spread_lit(
            env,
            file,
            loc.dupe(),
            *frozen,
            proto,
            elems,
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
        Value::ArrayLit(loc, t, ts) => {
            let reason = if as_const {
                reason::mk_reason(RConstArrayLit, loc.dupe())
            } else {
                reason::mk_reason(RArrayLit, loc.dupe())
            };
            let t = merge_impl(env, file, t, as_const, false);
            let ts: Vec<Type> = ts
                .iter()
                .map(|t| merge_impl(env, file, t, as_const, false))
                .collect();
            let elem_t = if ts.is_empty() {
                t.dupe()
            } else {
                let t0 = t.dupe();
                let t1 = ts[0].dupe();
                let rest: Vec<Type> = ts[1..].to_vec();
                let source_aloc = Some(file.cx.make_aloc_id(loc));
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
        Value::AsConst(value) => merge_value(env, file, value, true, false),
    }
}

fn merge_declare_module_implicitly_exported_object(
    env: &MergeEnv,
    file: &File,
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
            let p = merge_obj_value_prop(env, file, key, prop, true, false, false);
            (Name::new(key.dupe()), p)
        })
        .collect();
    obj_type::mk_with_proto(
        &file.cx,
        reason,
        type_::ObjKind::Exact,
        None,
        None,
        Some(props_map),
        None,
        proto,
    )
}

fn merge_object_lit(
    env: &MergeEnv,
    file: &File,
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
                &file.cx,
                proto_reason.dupe(),
                merge_impl(env, file, t, false, false),
            );
            type_util::typeof_annotation(proto_reason, proto, None)
        }
    };
    let props_map: type_::properties::PropertiesMap = props
        .iter()
        .map(|(key, prop)| {
            let p = merge_obj_value_prop(env, file, key, prop, for_export, as_const, frozen);
            (Name::new(key.dupe()), p)
        })
        .collect();
    obj_type::mk_with_proto(
        &file.cx,
        reason,
        type_::ObjKind::Exact,
        None,
        None,
        Some(props_map),
        None,
        proto_t,
    )
}

fn merge_obj_spread_lit(
    env: &MergeEnv,
    file: &File,
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
            let p = merge_obj_value_prop(env, file, key, prop, for_export, as_const, frozen);
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
            type_::object::spread::Operand::Type(merge_impl(env, file, t, as_const, false))
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
    annotation_inference::object_spread(&file.cx, use_op, reason, target, state, t)
}

fn merge_accessor(
    env: &MergeEnv,
    file: &File,
    accessor: &Accessor<ALoc, Pack::Packed<ALoc>>,
) -> type_::Property {
    match accessor {
        Accessor::Get(loc, t) => {
            let type_ = merge_impl(env, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Get {
                key_loc: Some(loc.dupe()),
                type_,
            })
        }
        Accessor::Set(loc, t) => {
            let type_ = merge_impl(env, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Set {
                key_loc: Some(loc.dupe()),
                type_,
            })
        }
        Accessor::GetSet(gloc, gt, sloc, st) => {
            let get_type = merge_impl(env, file, gt, false, false);
            let set_type = merge_impl(env, file, st, false, false);
            type_::Property::new(type_::PropertyInner::GetSet {
                get_key_loc: Some(gloc.dupe()),
                get_type,
                set_key_loc: Some(sloc.dupe()),
                set_type,
            })
        }
    }
}

fn merge_obj_value_prop(
    env: &MergeEnv,
    file: &File,
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
                file,
                move |type_, ref_loc, def_loc, value_name| {
                    // If name matches and prop loc is the name as ref loc, it must be shorthand syntax like
                    // module.exports = { foo }.
                    if key == *value_name && id_loc == ref_loc {
                        type_::Property::new(type_::PropertyInner::Field {
                            preferred_def_locs: Some(Vec1::new(def_loc)),
                            key_loc: Some(id_loc),
                            type_,
                            polarity,
                        })
                    } else {
                        type_::Property::new(type_::PropertyInner::Field {
                            preferred_def_locs: None,
                            key_loc: Some(id_loc),
                            type_,
                            polarity,
                        })
                    }
                },
                ref_,
                false,
            )
        }
        ObjValueProp::ObjValueField(id_loc, t, polarity) => {
            let type_ = merge_impl(env, file, t, as_const, false);
            let polarity = if as_const || frozen {
                Polarity::Positive
            } else {
                *polarity
            };
            type_::Property::new(type_::PropertyInner::Field {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_,
                polarity,
            })
        }
        ObjValueProp::ObjValueAccess(x) => merge_accessor(env, file, x),
        ObjValueProp::ObjValueMethod {
            id_loc,
            fn_loc,
            async_,
            generator,
            def,
        } => {
            let reason = reason::func_reason(*async_, *generator, fn_loc.dupe());
            let statics = merge_fun_statics(env, file, reason.dupe(), &BTreeMap::new());
            let type_ = merge_fun(env, file, reason, def, statics, false, false);
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_class_prop(
    env: &MergeEnv,
    file: &File,
    prop: &ObjValueProp<ALoc, Pack::Packed<ALoc>>,
) -> type_::Property {
    match prop {
        ObjValueProp::ObjValueField(id_loc, t, polarity) => {
            let type_ = merge_impl(env, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Field {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_,
                polarity: *polarity,
            })
        }
        ObjValueProp::ObjValueAccess(x) => merge_accessor(env, file, x),
        ObjValueProp::ObjValueMethod {
            id_loc,
            fn_loc,
            async_,
            generator,
            def,
        } => {
            let reason = reason::func_reason(*async_, *generator, fn_loc.dupe());
            let statics = type_::dummy_static(reason.dupe());
            let type_ = merge_fun(env, file, reason, def, statics, true, false);
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_obj_annot_prop(
    env: &MergeEnv,
    file: &File,
    prop: &ObjAnnotProp<ALoc, Pack::Packed<ALoc>>,
) -> type_::Property {
    use flow_common::reason::VirtualReasonDesc::*;
    match prop {
        ObjAnnotProp::ObjAnnotField(id_loc, t, polarity) => {
            let type_ = merge_impl(env, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Field {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_,
                polarity: *polarity,
            })
        }
        ObjAnnotProp::ObjAnnotAccess(x) => merge_accessor(env, file, x),
        ObjAnnotProp::ObjAnnotMethod {
            id_loc,
            fn_loc,
            def,
        } => {
            let reason = reason::mk_annot_reason(RFunctionType, fn_loc.dupe());
            let statics = merge_fun_statics(env, file, reason.dupe(), &BTreeMap::new());
            let type_ = merge_fun(env, file, reason, def, statics, false, false);
            type_::Property::new(type_::PropertyInner::Method {
                key_loc: Some(id_loc.dupe()),
                type_,
            })
        }
    }
}

fn merge_interface_prop(
    env: &MergeEnv,
    file: &File,
    prop: &InterfaceProp<ALoc, Pack::Packed<ALoc>>,
    is_static: bool,
) -> type_::Property {
    use flow_common::reason::VirtualReasonDesc::*;
    match prop {
        InterfaceProp::InterfaceField(id_loc, t, polarity) => {
            let t = merge_impl(env, file, t, false, false);
            type_::Property::new(type_::PropertyInner::Field {
                preferred_def_locs: None,
                key_loc: id_loc.dupe(),
                type_: t,
                polarity: *polarity,
            })
        }
        InterfaceProp::InterfaceAccess(x) => merge_accessor(env, file, x),
        InterfaceProp::InterfaceMethod(ms) => {
            let merge_method = |fn_loc: &ALoc, def: &FunSig<ALoc, Pack::Packed<ALoc>>| {
                let reason = reason::mk_reason(RFunctionType, fn_loc.dupe());
                let statics = type_::dummy_static(reason.dupe());
                merge_fun(env, file, reason, def, statics, true, is_static)
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

fn merge_dict(
    env: &MergeEnv,
    file: &File,
    dict: &ObjAnnotDict<Pack::Packed<ALoc>>,
    as_const: bool,
) -> type_::DictType {
    let key = merge_impl(env, file, &dict.key, false, false);
    let value = merge_impl(env, file, &dict.value, false, false);
    let polarity = Polarity::apply_const(as_const, dict.polarity);
    type_::DictType {
        dict_name: dict.name.dupe(),
        dict_polarity: polarity,
        key,
        value,
    }
}

fn merge_tparams_targs(
    env: &MergeEnv,
    file: &File,
    reason: Reason,
    t: impl FnOnce(&MergeEnv, Vec<(SubstName, Reason, Type, Polarity)>) -> Type,
    tparams: &TParams<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    match tparams {
        TParams::Mono => t(env, Vec::new()),
        TParams::Poly(tparams_loc, tps) => {
            let poly_reason = reason.update_desc(|d| RPolyType(Arc::new(d)));
            let mut current_env = env.dupe();
            let mut tparams_vec: Vec<type_::TypeParam> = Vec::new();
            let mut tparam_tuples: Vec<(SubstName, Reason, Type, Polarity)> = Vec::new();
            for tp in tps.iter() {
                let (tp, tuple) = merge_tparam(&mut current_env, file, tp, false);
                tparams_vec.push(tp);
                tparam_tuples.push(tuple);
            }
            let t_out = t(&current_env, tparam_tuples);
            let id = file.cx.make_source_poly_id(true, tparams_loc);
            Type::new(type_::TypeInner::DefT(
                poly_reason,
                type_::DefT::new(type_::DefTInner::PolyT {
                    tparams_loc: tparams_loc.dupe(),
                    tparams: tparams_vec.into(),
                    t_out,
                    id,
                }),
            ))
        }
    }
}

fn merge_tparam(
    env: &mut MergeEnv,
    file: &File,
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
        Some(t) => merge_impl(env, file, t, false, false),
    };
    let default = default
        .as_ref()
        .map(|t| merge_impl(env, file, t, false, false));
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
    let t = flow_js_utils::generic_of_tparam(&file.cx, |x: &Type| x.dupe(), &tp);
    env.tps.insert(name.dupe(), t.dupe());
    (tp, (SubstName::name(name.dupe()), reason, t, *polarity))
}

fn merge_op(env: &MergeEnv, file: &File, op: &Op<Box<Pack::Packed<ALoc>>>) -> Op<Type> {
    op.map(&mut (), |_, t| merge_impl(env, file, t, false, false))
}

fn merge_interface(
    env: &MergeEnv,
    file: &File,
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
        calls,
        dict,
    } = def;
    let super_ = {
        let super_reason = reason.dupe().update_desc(|d| RSuperOf(Arc::new(d)));
        let mut ts: Vec<Type> = extends
            .iter()
            .map(|t| merge_impl(env, file, t, false, false))
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
        let mut props = type_::properties::PropertiesMap::new();
        add_name_field(reason.dupe(), &mut props);
        let proto = Type::new(type_::TypeInner::NullProtoT(static_reason.dupe()));
        obj_type::mk_with_proto(
            &file.cx,
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
        let mut own = BTreeMap::new();
        let mut proto = BTreeMap::new();
        for (k, prop) in props {
            let t = merge_interface_prop(env, file, prop, false);
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
        (own.into(), proto.into())
    };
    let inst_call_t = {
        let ts: Vec<Type> = calls
            .iter()
            .map(|t| merge_impl(env, file, t, false, false))
            .collect();
        match ts.len() {
            0 => None,
            1 => Some(file.cx.make_call_prop(ts.into_iter().next().unwrap())),
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
                Some(file.cx.make_call_prop(t))
            }
        }
    };
    let inst_dict = dict.as_ref().map(|d| merge_dict(env, file, d, false));
    let inst = type_::InstType::new(type_::InstTypeInner {
        class_id: id,
        inst_react_dro: None,
        class_name,
        type_args: targs.into(),
        own_props: file.cx.generate_property_map(own_props),
        proto_props: file.cx.generate_property_map(proto_props),
        inst_call_t,
        initialized_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
        initialized_static_fields: flow_data_structure_wrapper::ord_set::FlowOrdSet::new(),
        inst_kind: type_::InstanceKind::InterfaceKind { inline },
        inst_dict,
        class_private_fields: file
            .cx
            .generate_property_map(type_::properties::PropertiesMap::new()),
        class_private_methods: file
            .cx
            .generate_property_map(type_::properties::PropertiesMap::new()),
        class_private_static_fields: file
            .cx
            .generate_property_map(type_::properties::PropertiesMap::new()),
        class_private_static_methods: file
            .cx
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

fn merge_class_extends(
    env: &MergeEnv,
    file: &File,
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
            let t = specialize(file, reason_op, merge_impl(env, file, t, false, false));
            let t = type_util::this_typeapp(t, this.dupe(), None, Some(loc.dupe()));
            let static_proto = type_util::class_type(t.dupe(), false, None);
            (t, static_proto)
        }
        ClassExtends::ClassExplicitExtendsApp { loc, t, targs } => {
            let t = merge_impl(env, file, t, false, false);
            let targs: Vec<Type> = targs
                .iter()
                .map(|targ| merge_impl(env, file, targ, false, false))
                .collect();
            let t = type_util::this_typeapp(t, this.dupe(), Some(targs), Some(loc.dupe()));
            let static_proto = type_util::class_type(t.dupe(), false, None);
            (t, static_proto)
        }
    };
    let mut all: Vec<Type> = mixins
        .iter()
        .map(|m| merge_class_mixin(env, file, this.dupe(), m))
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

fn merge_class_mixin(
    env: &MergeEnv,
    file: &File,
    this: Type,
    mixin: &ClassMixins<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    fn loop_mixin(file: &File, packed: &Pack::Packed<ALoc>) -> (Type, Vec<FlowSmolStr>) {
        match packed {
            Pack::Packed::Eval(loc, t, Op::GetProp(name)) => {
                let (t, mut names_rev) = loop_mixin(file, t);
                let t = eval(file, loc.dupe(), t, Op::GetProp(name.dupe()));
                names_rev.push(name.dupe());
                (t, names_rev)
            }
            Pack::Packed::Ref(r) => {
                let f = |t: Type, _ref_loc: ALoc, _def_loc: ALoc, name: &FlowSmolStr| {
                    (t, vec![name.dupe()])
                };
                merge_ref(file, f, r, false)
            }
            _ => panic!("unexpected class mixin"),
        }
    }

    let merge_mixin_ref = |file: &File, loc: ALoc, packed: &Pack::Packed<ALoc>| -> Type {
        let (t, names_rev) = loop_mixin(file, packed);
        let names: Vec<&str> = names_rev.iter().map(|s| s.as_str()).collect();
        let name = names.join(".");
        let reason = reason::mk_annot_reason(RType(Name::new(FlowSmolStr::new(&name))), loc);
        annotation_inference::mixin(&file.cx, reason, t)
    };

    match mixin {
        ClassMixins::ClassMixin { loc, t } => {
            let reason_op = reason::mk_reason(RClassMixins, loc.dupe());
            let t = specialize(file, reason_op, merge_mixin_ref(file, loc.dupe(), t));
            type_util::this_typeapp(t, this, None, Some(loc.dupe()))
        }
        ClassMixins::ClassMixinApp { loc, t, targs } => {
            let t = merge_mixin_ref(file, loc.dupe(), t);
            let targs: Vec<Type> = targs
                .iter()
                .map(|targ| merge_impl(env, file, targ, false, false))
                .collect();
            type_util::this_typeapp(t, this, Some(targs), Some(loc.dupe()))
        }
    }
}

fn merge_this_class_t(
    file: File,
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
        tparams: _,
    } = def;
    let class_name_owned = class_name;
    move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>, rec_type: Type| {
        let file = &file;
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
            flow_js_utils::generic_of_tparam(&file.cx, |x| x.dupe(), &this_tp)
        };
        let (super_, static_proto) =
            merge_class_extends(env, file, this.dupe(), reason.dupe(), &extends, &[]);
        let implements: Vec<Type> = implements
            .iter()
            .map(|t| merge_impl(env, file, t, false, false))
            .collect();
        let mut env = env.dupe();
        env.tps.insert(FlowSmolStr::new_inline("this"), this.dupe());
        // let static =
        let static_ = {
            let static_reason = reason.dupe().update_desc(|d| RStatics(Arc::new(d)));
            let mut props = type_::properties::PropertiesMap::new();
            for (k, prop) in static_props {
                let p = merge_class_prop(&env, file, &prop);
                props.insert(Name::new(k.dupe()), p);
            }
            add_name_field(reason.dupe(), &mut props);
            obj_type::mk_with_proto(
                &file.cx,
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
            let p = merge_class_prop(&env, file, &prop);
            own_props_map.insert(k.dupe(), p);
        }
        let mut proto_props_map: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
        for (k, prop) in proto_props {
            let p = merge_class_prop(&env, file, &prop);
            proto_props_map.insert(k.dupe(), p);
        }
        match &inst_kind {
            type_::InstanceKind::RecordKind { defaulted_props } => add_record_constructor(
                file,
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
        let own_props = file.cx.generate_property_map(own_props_pmap);
        add_default_constructor(reason.dupe(), &extends, &mut proto_props_map);
        let proto_props_pmap: type_::properties::PropertiesMap = proto_props_map
            .into_iter()
            .map(|(k, v)| (Name::new(k), v))
            .collect();
        let proto_props = file.cx.generate_property_map(proto_props_pmap);
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
            inst_dict: None,
            class_private_fields: file
                .cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_methods: file
                .cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_static_fields: file
                .cx
                .generate_property_map(type_::properties::PropertiesMap::new()),
            class_private_static_methods: file
                .cx
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

fn merge_class(
    env: &MergeEnv,
    file: &File,
    reason: Reason,
    class_name: Option<&FlowSmolStr>,
    id: flow_aloc::ALocId,
    def: &ClassSig<ALoc, Pack::Packed<ALoc>>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let tparams = &def.tparams;
    let this_reason = reason.dupe().replace_desc(RThisType);
    let this_class_t = merge_this_class_t(
        file.dupe(),
        reason.dupe(),
        class_name.map(|n| n.dupe()),
        id,
        def.clone(),
        this_reason.dupe(),
        type_::InstanceKind::ClassKind,
    );
    let this_reason_c = this_reason.dupe();
    let t = move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let lazy_cell: Rc<RefCell<Option<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>>> =
            Rc::new(RefCell::new(None));

        let lazy_cell_c = lazy_cell.dupe();
        let this_reason_for_lazy = this_reason_c.dupe();
        let file_cx = file.cx.dupe();
        let env_clone = env.dupe();
        let lazy_val: Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> =
            Rc::new(Lazy::new(Box::new(move || {
                let self_ref = lazy_cell_c.borrow().as_ref().unwrap().dupe();
                let rec_type = flow_typing_tvar::mk_fully_resolved_lazy(
                    &file_cx,
                    this_reason_for_lazy,
                    false,
                    self_ref,
                );
                this_class_t(&env_clone, targs, rec_type)
            }) as Box<dyn FnOnce() -> Type>));

        *lazy_cell.borrow_mut() = Some(lazy_val.dupe());
        (*lazy_val).dupe()
    };
    merge_tparams_targs(env, file, reason, t, tparams)
}

fn merge_record(
    env: &MergeEnv,
    file: &File,
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
    let t = move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let lazy_cell: Rc<RefCell<Option<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>>> =
            Rc::new(RefCell::new(None));

        let lazy_cell_c = lazy_cell.dupe();
        let this_reason_for_lazy = this_reason_c.dupe();
        let file_cx = file.cx.dupe();
        let env_clone = env.dupe();
        let lazy_val: Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> =
            Rc::new(Lazy::new(Box::new(move || {
                let self_ref = lazy_cell_c.borrow().as_ref().unwrap().dupe();
                let rec_type = flow_typing_tvar::mk_fully_resolved_lazy(
                    &file_cx,
                    this_reason_for_lazy,
                    false,
                    self_ref,
                );
                this_class_t(&env_clone, targs, rec_type)
            }) as Box<dyn FnOnce() -> Type>));

        *lazy_cell.borrow_mut() = Some(lazy_val.dupe());
        (*lazy_val).dupe()
    };
    merge_tparams_targs(env, file, reason, t, tparams)
}

fn merge_fun_statics(
    env: &MergeEnv,
    file: &File,
    reason: Reason,
    statics: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>,
) -> Type {
    let props: type_::properties::PropertiesMap = statics
        .iter()
        .map(|(key, (id_loc, t))| {
            let t = merge_impl(env, file, t, false, false);
            let prop = type_::Property::new(type_::PropertyInner::Field {
                preferred_def_locs: None,
                key_loc: Some(id_loc.dupe()),
                type_: t,
                polarity: Polarity::Neutral,
            });
            (Name::new(key.dupe()), prop)
        })
        .collect();
    let reason = reason.update_desc(|d| reason::VirtualReasonDesc::RStatics(Arc::new(d)));
    obj_type::mk_with_proto(
        &file.cx,
        reason.dupe(),
        type_::ObjKind::Inexact,
        None,
        None,
        Some(props),
        None,
        Type::new(type_::TypeInner::FunProtoT(reason)),
    )
}

fn merge_fun(
    env: &MergeEnv,
    file: &File,
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
    let t = |env: &MergeEnv, _tparam_tuples: Vec<(SubstName, Reason, Type, Polarity)>| {
        let params: Vec<type_::FunParam> = def_ref
            .params
            .iter()
            .map(|param| {
                let t = merge_impl(env, file, &param.t, false, false);
                type_::FunParam(param.name.dupe(), t)
            })
            .collect();
        let rest_param = def_ref.rest_param.as_ref().map(|rp| {
            let t = merge_impl(env, file, &rp.t, false, false);
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
            Some(t) => merge_impl(env, file, t, false, false),
        };
        let return_t = merge_impl(env, file, &def_ref.return_, false, false);
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
                        type_guard: merge_impl(env, file, t, false, false),
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
                type_::ReactEffectType::HookDecl(file.cx.make_aloc_id(l))
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
    merge_tparams_targs(env, file, reason, t, &def.tparams)
}

fn merge_component(
    env: &MergeEnv,
    file: &File,
    reason: Reason,
    is_annotation: bool,
    def: &ComponentSig<ALoc, Pack::Packed<ALoc>>,
    id_opt: Option<(ALoc, &FlowSmolStr)>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    // let t (env, _) =
    let reason2 = reason.dupe();
    let t = |env: &MergeEnv, _: Vec<(SubstName, Reason, Type, Polarity)>| -> Type {
        let pmap: type_::properties::PropertiesMap = def
            .params
            .iter()
            .map(|param| {
                let ComponentParam { name, name_loc, t } = param;
                let t = merge_impl(env, file, t, false, false);
                (
                    Name::new(name.dupe()),
                    type_::Property::new(type_::PropertyInner::Field {
                        preferred_def_locs: None,
                        key_loc: Some(name_loc.dupe()),
                        type_: t,
                        polarity: Polarity::Positive,
                    }),
                )
            })
            .collect();
        let config_reason = reason::mk_reason(
            RPropsOfComponent(Arc::new(reason2.desc(false).clone())),
            def.params_loc.dupe(),
        );
        let rest_t = match &def.rest_param {
            None => obj_type::mk_with_proto(
                &file.cx,
                config_reason.dupe(),
                type_::ObjKind::Exact,
                None,
                None,
                None,
                None,
                Type::new(type_::TypeInner::ObjProtoT(config_reason.dupe())),
            ),
            Some(ComponentRestParam { t }) => merge_impl(env, file, t, false, false),
        };
        let allow_ref_in_spread = match file.cx.react_ref_as_prop() {
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
        let renders = merge_impl(env, file, &def.renders, false, false);
        let component_kind = match id_opt {
            None => type_::ComponentKind::Structural,
            Some((loc, name)) => {
                let id = file.cx.make_aloc_id(&loc);
                type_::ComponentKind::Nominal(id, name.dupe(), None)
            }
        };
        Type::new(type_::TypeInner::DefT(
            reason2.dupe(),
            type_::DefT::new(type_::DefTInner::ReactAbstractComponentT {
                config: param,
                renders,
                component_kind,
            }),
        ))
    };
    merge_tparams_targs(env, file, reason, t, &def.tparams)
}

fn merge_type_alias(
    file: &File,
    reason: Reason,
    custom_error_loc_opt: Option<ALoc>,
    name: &FlowSmolStr,
    tparams: &TParams<ALoc, Pack::Packed<ALoc>>,
    body: &Pack::Packed<ALoc>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;

    let name_owned = name.dupe();
    let reason_clone = reason.dupe();
    let t = move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let t = merge_impl(env, file, body, false, false);
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
                    file.cx.make_aloc_id(&id_loc),
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
    merge_tparams_targs(&mk_merge_env(FlowOrdMap::new()), file, reason, t, tparams)
}

fn merge_opaque_type(
    file: &File,
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
    let t = move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let opaque_reason = reason_clone
            .dupe()
            .replace_desc(ROpaqueType(name_owned.dupe()));
        let lower_t = lower_bound.map(|t| merge_impl(env, file, t, false, false));
        let upper_t = upper_bound.map(|t| merge_impl(env, file, t, false, false));
        let underlying_t = match body {
            None => type_::nominal::UnderlyingT::FullyOpaque,
            Some(t) => type_::nominal::UnderlyingT::OpaqueWithLocal {
                t: merge_impl(env, file, t, false, false),
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
    merge_tparams_targs(&mk_merge_env(FlowOrdMap::new()), file, reason, t, tparams)
}

fn merge_declare_class(
    file: &File,
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
        static_calls,
        calls,
        dict,
        static_dict: _,
    } = def.clone();
    let this_reason = reason.dupe().replace_desc(RThisType);
    let class_name_owned = class_name.dupe();
    let id_owned = id.dupe();
    let reason_c = reason.dupe();
    let this_reason_c = this_reason.dupe();
    let file_c = file.dupe();
    let this_class_t =
        move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>, rec_type: Type| {
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
                flow_js_utils::generic_of_tparam(&file.cx, |x| x.dupe(), &this_tp)
            };
            let (super_, static_proto) =
                merge_class_extends(env, file, this.dupe(), reason_c.dupe(), &extends, &mixins);
            let implements: Vec<Type> = implements
                .iter()
                .map(|t| merge_impl(env, file, t, false, false))
                .collect();
            let mut env = env.dupe();
            env.tps.insert(FlowSmolStr::new_inline("this"), this.dupe());
            let static_ = {
                let static_reason = reason_c.dupe().update_desc(|d| RStatics(Arc::new(d)));
                let mut props = type_::properties::PropertiesMap::new();
                for (k, prop) in static_props {
                    let p = merge_interface_prop(&env, file, &prop, true);
                    props.insert(Name::new(k.dupe()), p);
                }
                add_name_field(reason_c.dupe(), &mut props);
                let call = {
                    let ts: Vec<Type> = static_calls
                        .iter()
                        .map(|t| merge_impl(&env, file, t, false, false))
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
                    &file.cx,
                    static_reason,
                    type_::ObjKind::Inexact,
                    None,
                    call,
                    Some(props),
                    None,
                    static_proto,
                )
            };
            let own_props = {
                let pmap: type_::properties::PropertiesMap = own_props
                    .iter()
                    .map(|(k, prop)| {
                        let p = merge_interface_prop(&env, file, prop, false);
                        (Name::new(k.dupe()), p)
                    })
                    .collect();
                file.cx.generate_property_map(pmap)
            };
            let proto_props = {
                let mut proto_map: BTreeMap<FlowSmolStr, type_::Property> = BTreeMap::new();
                for (k, prop) in proto_props {
                    let p = merge_interface_prop(&env, file, &prop, false);
                    proto_map.insert(k.dupe(), p);
                }
                add_default_constructor(reason_c.dupe(), &extends, &mut proto_map);
                let pmap: type_::properties::PropertiesMap = proto_map
                    .into_iter()
                    .map(|(k, v)| (Name::new(k), v))
                    .collect();
                file.cx.generate_property_map(pmap)
            };
            let inst_call_t = {
                let ts: Vec<Type> = calls
                    .iter()
                    .map(|t| merge_impl(&env, file, t, false, false))
                    .collect();
                match ts.len() {
                    0 => None,
                    1 => Some(file.cx.make_call_prop(ts.into_iter().next().unwrap())),
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
                        Some(file.cx.make_call_prop(t))
                    }
                }
            };
            let inst_dict = dict.as_ref().map(|d| merge_dict(&env, file, d, false));
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
                class_private_fields: file
                    .cx
                    .generate_property_map(type_::properties::PropertiesMap::new()),
                class_private_methods: file
                    .cx
                    .generate_property_map(type_::properties::PropertiesMap::new()),
                class_private_static_fields: file
                    .cx
                    .generate_property_map(type_::properties::PropertiesMap::new()),
                class_private_static_methods: file
                    .cx
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
    let t = move |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>| {
        let lazy_cell: Rc<RefCell<Option<Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>>>> =
            Rc::new(RefCell::new(None));

        let lazy_cell_c = lazy_cell.dupe();
        let this_reason_for_lazy = this_reason_t.dupe();
        let file_cx = file.cx.dupe();
        let env_clone = env.dupe();
        let lazy_val: Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> =
            Rc::new(Lazy::new(Box::new(move || {
                let self_ref = lazy_cell_c.borrow().as_ref().unwrap().dupe();
                let rec_type = flow_typing_tvar::mk_fully_resolved_lazy(
                    &file_cx,
                    this_reason_for_lazy,
                    false,
                    self_ref,
                );
                this_class_t(&env_clone, targs, rec_type)
            }) as Box<dyn FnOnce() -> Type>));

        *lazy_cell.borrow_mut() = Some(lazy_val.dupe());
        (*lazy_val).dupe()
    };
    merge_tparams_targs(&mk_merge_env(FlowOrdMap::new()), file, reason, t, &tparams)
}

fn merge_declare_fun(
    file: &File,
    defs: &Vec1<(ALoc, ALoc, FunSig<ALoc, Pack::Packed<ALoc>>)>,
) -> Type {
    use flow_common::reason::VirtualReasonDesc::*;
    let ts: Vec<Type> = defs
        .iter()
        .map(|(_, fn_loc, def)| {
            let reason = reason::mk_reason(RFunctionType, fn_loc.dupe());
            let env = mk_merge_env(FlowOrdMap::new());
            let statics = merge_fun_statics(&env, file, reason.dupe(), &BTreeMap::new());
            merge_fun(&env, file, reason, def, statics, false, false)
        })
        .collect();
    match ts.len() {
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
    }
}

pub fn merge_def(
    file: &File,
    reason: Reason,
    def: &Def<ALoc, Pack::Packed<ALoc>>,
    const_decl: bool,
) -> Type {
    match def {
        Def::TypeAlias {
            id_loc: _,
            custom_error_loc_opt,
            name,
            tparams,
            body,
        } => merge_type_alias(
            file,
            reason,
            custom_error_loc_opt.dupe(),
            name,
            tparams,
            body,
        ),
        Def::OpaqueType {
            id_loc,
            name,
            tparams,
            body,
            lower_bound,
            upper_bound,
        } => {
            let id = type_::nominal::Id::UserDefinedOpaqueTypeId(
                file.cx.make_aloc_id(id_loc),
                name.dupe(),
            );
            merge_opaque_type(
                file,
                reason,
                id,
                name,
                tparams,
                lower_bound.as_ref(),
                upper_bound.as_ref(),
                body.as_ref(),
            )
        }
        Def::Interface {
            id_loc,
            name,
            tparams,
            def,
        } => {
            let id = file.cx.make_aloc_id(id_loc);
            let t = |env: &MergeEnv, targs: Vec<(SubstName, Reason, Type, Polarity)>| {
                let t = merge_interface(
                    env,
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
                file,
                reason.dupe(),
                t,
                tparams,
            )
        }
        Def::ClassBinding { id_loc, name, def } => {
            let id = file.cx.make_aloc_id(id_loc);
            merge_class(
                &mk_merge_env(FlowOrdMap::new()),
                file,
                reason,
                Some(name),
                id,
                def,
            )
        }
        Def::DeclareClassBinding {
            id_loc: _,
            nominal_id_loc,
            name,
            def,
        } => {
            let nominal_id = file.cx.make_aloc_id(nominal_id_loc);
            merge_declare_class(file, reason, name, nominal_id, def)
        }
        Def::RecordBinding {
            id_loc,
            name,
            def,
            defaulted_props,
        } => {
            let id = file.cx.make_aloc_id(id_loc);
            merge_record(
                &mk_merge_env(FlowOrdMap::new()),
                file,
                reason,
                Some(name),
                id,
                def,
                defaulted_props,
            )
        }
        Def::FunBinding {
            id_loc: _,
            name: _,
            async_: _,
            generator: _,
            fn_loc: _,
            def,
            statics,
        } => {
            let env = mk_merge_env(FlowOrdMap::new());
            let statics_t = merge_fun_statics(&env, file, reason.dupe(), statics);
            merge_fun(&env, file, reason, def, statics_t, false, false)
        }
        Def::DeclareFun {
            id_loc,
            fn_loc,
            name: _,
            def,
            tail,
        } => {
            let mut all_defs = vec![(id_loc.dupe(), fn_loc.dupe(), def.clone())];
            all_defs.extend(tail.iter().cloned());
            let defs = Vec1::try_from_vec(all_defs).unwrap();
            merge_declare_fun(file, &defs)
        }
        Def::ComponentBinding {
            id_loc,
            name,
            fn_loc: _,
            def,
        } => {
            let env = mk_merge_env(FlowOrdMap::new());
            merge_component(&env, file, reason, false, def, Some((id_loc.dupe(), name)))
        }
        Def::Variable {
            id_loc: _,
            name: _,
            def,
        } => {
            let env = mk_merge_env(FlowOrdMap::new());
            merge_impl(&env, file, def, false, const_decl)
        }
        Def::Parameter {
            id_loc: _,
            name: _,
            def,
            tparams,
        } => {
            let t = |env: &MergeEnv, _targs: Vec<(SubstName, Reason, Type, Polarity)>| {
                merge_impl(env, file, def, false, const_decl)
            };
            merge_tparams_targs(&mk_merge_env(FlowOrdMap::new()), file, reason, t, tparams)
        }
        Def::DisabledComponentBinding { .. }
        | Def::DisabledEnumBinding { .. }
        | Def::DisabledRecordBinding { .. } => type_::any_t::error(reason),
        Def::EnumBinding {
            id_loc,
            rep,
            members,
            has_unknown_members,
            name,
        } => merge_enum(
            file,
            reason,
            id_loc.dupe(),
            name,
            rep,
            members,
            *has_unknown_members,
        ),
        Def::NamespaceBinding {
            id_loc,
            name,
            values,
            types,
        } => {
            let f = |smap: &BTreeMap<FlowSmolStr, (ALoc, Pack::Packed<ALoc>)>| {
                let mut result: BTreeMap<Name, type_::NamedSymbol> = BTreeMap::new();
                for (name, (loc, packed)) in smap {
                    let t =
                        merge_impl(&mk_merge_env(FlowOrdMap::new()), file, packed, false, false);
                    result.insert(
                        Name::new(name.dupe()),
                        type_::NamedSymbol::new(Some(loc.dupe()), None, t),
                    );
                }
                result
            };
            let namespace_symbol = Symbol::mk_namespace_symbol(name.dupe(), id_loc.dupe());
            flow_js_utils::namespace_type(&file.cx, reason, namespace_symbol, &f(values), &f(types))
        }
    }
}

pub fn merge_export(file: &File, export: &Pack::Export<ALoc>) -> NamedSymbol {
    match export {
        Pack::Export::ExportRef(ref_) => merge_ref(
            file,
            |type_, _ref_loc, def_loc, _name| NamedSymbol::new(Some(def_loc), None, type_),
            ref_,
            false,
        ),
        Pack::Export::ExportDefault { default_loc, def } => {
            if let Pack::Packed::Ref(ref_) = def {
                merge_ref(
                    file,
                    |type_, _ref_loc, def_loc, _name| NamedSymbol::new(Some(def_loc), None, type_),
                    ref_,
                    false,
                )
            } else {
                let type_ = merge_impl(&mk_merge_env(FlowOrdMap::new()), file, def, false, false);
                NamedSymbol::new(Some(default_loc.dupe()), None, type_)
            }
        }
        Pack::Export::ExportBinding(index) => {
            let entry = Lazy::force(file.local_defs.get(*index));
            let (loc, _name, _t_general, t_const) = entry;
            let type_ = Lazy::force(&**t_const).dupe();
            NamedSymbol::new(Some(loc.dupe()), None, type_)
        }
        Pack::Export::ExportDefaultBinding {
            default_loc: _,
            index,
        } => {
            let entry = Lazy::force(file.local_defs.get(*index));
            let (loc, _name, _t_general, t_const) = entry;
            let type_ = Lazy::force(&**t_const).dupe();
            NamedSymbol::new(Some(loc.dupe()), None, type_)
        }
        Pack::Export::ExportFrom(index) => {
            let entry = Lazy::force(file.remote_refs.get(*index));
            let (loc, _name, type_) = entry;
            NamedSymbol::new(Some(loc.dupe()), None, type_.dupe())
        }
    }
}

pub fn merge_resource_module_t(
    cx: &Context,
    file_key: FileKey,
    filename: &str,
) -> (
    Reason,
    Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>>,
) {
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

pub fn merge(tps: TparamsMap, file: &File, packed: &Pack::Packed<ALoc>) -> Type {
    let env = mk_merge_env(tps);
    merge_impl(&env, file, packed, false, false)
}

pub fn merge_cjs_export_t(file: &File, packed: &Pack::Packed<ALoc>) -> (Option<ALoc>, Type) {
    use flow_type_sig::type_sig::Value;
    // We run a special code path for objects in cjs exports,
    // in order to retain the original definition location of exported names
    match packed {
        Pack::Packed::Value(value) => match value.as_ref() {
            Value::ObjLit {
                loc,
                frozen,
                proto,
                props,
            } => {
                let env = mk_merge_env(FlowOrdMap::new());
                (
                    Some(loc.dupe()),
                    merge_object_lit(&env, file, loc.dupe(), *frozen, proto, props, true, false),
                )
            }
            Value::ObjSpreadLit {
                loc,
                frozen,
                proto,
                elems,
            } => {
                let env = mk_merge_env(FlowOrdMap::new());
                (
                    Some(loc.dupe()),
                    merge_obj_spread_lit(
                        &env,
                        file,
                        loc.dupe(),
                        *frozen,
                        proto,
                        elems,
                        true,
                        false,
                    ),
                )
            }
            _ => (None, merge(FlowOrdMap::new(), file, packed)),
        },
        Pack::Packed::Ref(ref_) => merge_ref(
            file,
            |type_, _ref_loc, def_loc, _name| (Some(def_loc), type_),
            ref_,
            false,
        ),
        _ => (None, merge(FlowOrdMap::new(), file, packed)),
    }
}

pub fn merge_builtins(
    cx: &Context,
    file_key: FileKey,
    builtin_locs: Arc<Table<Loc>>,
    builtins: Arc<packed_type_sig::Builtins<Loc>>,
) -> (
    BTreeMap<FlowSmolStr, Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>>,
    BTreeMap<FlowSmolStr, Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>>,
    BTreeMap<
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
) {
    annotation_inference::set_dst_cx(cx);

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

    type FileAndDepMap = (
        File,
        BTreeMap<
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
    );
    type LazyFileAndDepMap = Rc<Lazy<FileAndDepMap, Box<dyn FnOnce() -> FileAndDepMap>>>;

    type LocalDefEntry = (
        ALoc,
        FlowSmolStr,
        Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
        Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
    );

    fn local_def_impl(
        cx: &Context,
        aloc: &Rc<impl Fn(&Index<Loc>) -> ALoc + 'static>,
        file_and_dependency_map_rec: &LazyFileAndDepMap,
        def: &Pack::PackedDef<Index<Loc>>,
    ) -> Rc<Lazy<LocalDefEntry, Box<dyn FnOnce() -> LocalDefEntry>>> {
        let aloc = aloc.dupe();
        let def = def.clone();
        let cx = cx.dupe();
        let file_and_dep = file_and_dependency_map_rec.dupe();
        Rc::new(Lazy::new(Box::new(move || {
            let def = def.map(
                &mut (),
                |_, loc: &Index<Loc>| (*aloc)(loc),
                |_, t: &Pack::Packed<Index<Loc>>| t.map(&|i| (*aloc)(i)),
            );
            let loc = def.id_loc();
            let name = def.name().dupe();
            let reason = def_reason(&def);
            let make_type = |const_decl: bool| -> Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> {
                let file_and_dep = file_and_dep.dupe();
                let reason = reason.dupe();
                let reason_for_tvar = reason.dupe();
                let def = def.clone();
                let cx = cx.dupe();
                Rc::new(Lazy::new(Box::new(move || {
                    let resolved: Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> =
                        Rc::new(Lazy::new(Box::new(move || {
                            let (file, _) = Lazy::force(&*file_and_dep);
                            merge_def(file, reason, &def, const_decl)
                        })));
                    annotation_inference::mk_sig_tvar(&cx, reason_for_tvar, resolved)
                })))
            };
            (loc, name, make_type(false), make_type(true))
        }) as Box<dyn FnOnce() -> LocalDefEntry>))
    }

    let remote_ref_fn = {
        let cx = cx.dupe();
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap,
              rref: &Pack::RemoteRef<Index<Loc>>|
              -> Rc<
            Lazy<(ALoc, FlowSmolStr, Type), Box<dyn FnOnce() -> (ALoc, FlowSmolStr, Type)>>,
        > {
            let aloc = aloc.dupe();
            let rref = rref.clone();
            let cx = cx.dupe();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            Rc::new(Lazy::new(Box::new(move || {
                let rref = rref.map(&|i| (*aloc)(i));
                let loc = rref.loc().dupe();
                let name = rref.name().dupe();
                let reason = remote_ref_reason(&rref);
                let file_and_dep2 = file_and_dep.dupe();
                let reason2 = reason.dupe();
                let rref2 = rref.clone();
                let resolved: Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> =
                    Rc::new(Lazy::new(Box::new(move || {
                        let (file, _) = Lazy::force(&*file_and_dep2);
                        merge_remote_ref(file, reason2, &rref2)
                    })));
                let t = annotation_inference::mk_sig_tvar(&cx, reason.dupe(), resolved);
                (loc, name, t)
            })
                as Box<dyn FnOnce() -> (ALoc, FlowSmolStr, Type)>))
        }
    };

    let pattern_def_fn = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap,
              def: &Pack::Packed<Index<Loc>>|
              -> Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> {
            let aloc = aloc.dupe();
            let def = def.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            Rc::new(Lazy::new(Box::new(move || {
                let def = def.map(&|i| (*aloc)(i));
                let (file, _) = Lazy::force(&*file_and_dep);
                merge(FlowOrdMap::new(), file, &def)
            }) as Box<dyn FnOnce() -> Type>))
        }
    };

    let pattern_fn = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap,
              p: &Pack::Pattern<Index<Loc>>|
              -> Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>> {
            let aloc = aloc.dupe();
            let p = p.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            Rc::new(Lazy::new(Box::new(move || {
                let p = p.map(&|i| (*aloc)(i));
                let (file, _) = Lazy::force(&*file_and_dep);
                merge_pattern(file, &p)
            }) as Box<dyn FnOnce() -> Type>))
        }
    };

    let map_module = {
        let aloc = aloc.dupe();
        move |file_and_dependency_map_rec: &LazyFileAndDepMap,
              module_loc: ALoc,
              module_kind: &Pack::ModuleKind<Index<Loc>>|
              -> Rc<
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
        > {
            use flow_common::reason::VirtualReasonDesc::*;
            let reason = reason::mk_reason(RExports, module_loc);

            let aloc_fn = aloc.dupe();
            let module_kind = module_kind.clone();
            let file_and_dep = file_and_dependency_map_rec.dupe();
            let reason2 = reason.dupe();
            Rc::new(Lazy::new(Box::new(move || {
                let type_export =
                    |export: &Pack::TypeExport<Index<Loc>>| -> Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>> {
                        let export = export.clone();
                        let aloc_fn = aloc_fn.dupe();
                        let file_and_dep = file_and_dep.dupe();
                        let reason = reason2.dupe();
                        Rc::new(Lazy::new(Box::new(move || {
                            let export = export.map(&*aloc_fn);
                            let (file, _) = Lazy::force(&*file_and_dep);
                            merge_type_export(file, reason, &export)
                        })))
                    };
                let cjs_exports_fn = |export: Pack::Packed<Index<Loc>>| -> Rc<
                    Lazy<(Option<ALoc>, Type), Box<dyn FnOnce() -> (Option<ALoc>, Type)>>,
                > {
                    let aloc_fn = aloc_fn.dupe();
                    let file_and_dep = file_and_dep.dupe();
                    Rc::new(Lazy::new(Box::new(move || {
                        let export = export.map(&*aloc_fn);
                        let (file, _) = Lazy::force(&*file_and_dep);
                        merge_cjs_export_t(file, &export)
                    })))
                };
                let es_export_fn =
                    |export: &Pack::Export<Index<Loc>>| -> Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>> {
                        let export = export.clone();
                        let aloc_fn = aloc_fn.dupe();
                        let file_and_dep = file_and_dep.dupe();
                        Rc::new(Lazy::new(Box::new(move || {
                            let export = export.map(&*aloc_fn);
                            let (file, _) = Lazy::force(&*file_and_dep);
                            merge_export(file, &export)
                        })))
                    };
                let cjs_module = |type_exports: &[Pack::TypeExport<Index<Loc>>],
                                  exports: Option<Pack::Packed<Index<Loc>>>,
                                  info: Pack::CJSModuleInfo<Index<Loc>>|
                 -> Exports {
                    let info = info.map(&*aloc_fn);
                    let Pack::CJSModuleInfo {
                        type_export_keys,
                        type_stars,
                        strict,
                        platform_availability_set,
                    } = info;
                    let type_exports = type_exports.iter().map(&type_export).collect::<Vec<_>>();
                    let exports = exports.map(&cjs_exports_fn);
                    let type_exports: BTreeMap<
                        FlowSmolStr,
                        Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>>,
                    > = type_export_keys.into_iter().zip(type_exports).collect();
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
                 -> Exports {
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
                    let type_exports: BTreeMap<
                        FlowSmolStr,
                        Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>>,
                    > = type_export_keys.into_iter().zip(type_exports).collect();
                    let exports = exports.iter().map(es_export_fn).collect::<Vec<_>>();
                    let exports: BTreeMap<
                        FlowSmolStr,
                        Rc<Lazy<NamedSymbol, Box<dyn FnOnce() -> NamedSymbol>>>,
                    > = export_keys.into_iter().zip(exports).collect();
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
                let (file, _) = Lazy::force(&*file_and_dep);
                (reason2.dupe(), merge_exports(file, reason2, info))
            })
                as Box<
                    dyn FnOnce() -> (
                        Reason,
                        Rc<Lazy<ModuleType, Box<dyn FnOnce() -> ModuleType>>>,
                    ),
                >))
        }
    };

    let file_and_dependency_map_rec: LazyFileAndDepMap = {
        use std::cell::OnceCell;
        let cell: Rc<OnceCell<FileAndDepMap>> = Rc::new(OnceCell::new());
        let cell_ref = cell.dupe();
        let cx = cx.dupe();
        let aloc = aloc.dupe();
        let builtins = builtins.dupe();
        Rc::new(Lazy::new(Box::new(move || {
            if let Some(val) = cell_ref.get() {
                return (val.0.dupe(), val.1.clone());
            }
            let self_ref: LazyFileAndDepMap = {
                let cell_ref2 = cell_ref.dupe();
                Rc::new(Lazy::new(Box::new(move || {
                    let val = cell_ref2.get().expect("recursive lazy not yet initialized");
                    (val.0.dupe(), val.1.clone())
                })
                    as Box<dyn FnOnce() -> FileAndDepMap>))
            };

            let mut dependencies_map: BTreeMap<
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
            >|
             -> Rc<
                Lazy<ResolvedRequire, Box<dyn FnOnce() -> ResolvedRequire>>,
            > {
                match dependencies_map.get(specifier.as_str()) {
                    None => Rc::new(Lazy::new(Box::new(|| ResolvedRequire::MissingModule)
                        as Box<dyn FnOnce() -> ResolvedRequire>)),
                    Some(lazy_module) => {
                        let lazy_module = lazy_module.dupe();
                        let cx = cx.dupe();
                        Rc::new(Lazy::new(Box::new(move || {
                            let (r, module_type_lazy) = Lazy::force(&*lazy_module);
                            let r = r.dupe();
                            let module_type_lazy = module_type_lazy.dupe();
                            let forcing_state =
                                type_::constraint::forcing_state::State::<
                                    Result<ModuleType, Type>,
                                    Reason,
                                >::of_lazy_t(r, move || {
                                    let mt = Lazy::force(&*module_type_lazy);
                                    Ok(mt.dupe())
                                });
                            let cx = cx.dupe();
                            ResolvedRequire::TypedModule(Rc::new(move || {
                                forcing_state
                                    .force(|r| Err(annotation_inference::error_recursive(&cx, r)))
                            }))
                        })
                            as Box<dyn FnOnce() -> ResolvedRequire>))
                    }
                }
            };

            use flow_common::reason::VirtualReasonDesc::*;
            let exports: Rc<dyn Fn() -> Result<ModuleType, Type>> =
                Rc::new(|| Err(type_::any_t::annot(reason::locationless_reason(RExports))));

            let file = File(Rc::new(FileInner {
                cx: cx.dupe(),
                dependencies: builtins.module_refs.map(|mref| {
                    let resolved = map_module_ref(mref, &dependencies_map);
                    (mref.dupe(), resolved)
                }),
                exports,
                local_defs: builtins
                    .local_defs
                    .map(|def| local_def_impl(&cx, &aloc, &self_ref, def)),
                remote_refs: builtins
                    .remote_refs
                    .map(|rref| remote_ref_fn(&self_ref, rref)),
                pattern_defs: builtins
                    .pattern_defs
                    .map(|def| pattern_def_fn(&self_ref, def)),
                patterns: builtins.patterns.map(|p| pattern_fn(&self_ref, p)),
            }));

            let result = (file, dependencies_map);
            let _ = cell_ref.set(result.clone());
            result
        }) as Box<dyn FnOnce() -> FileAndDepMap>))
    };
    let builtin_values: BTreeMap<
        FlowSmolStr,
        Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>,
    > = builtins
        .global_values
        .iter()
        .map(|(name, i)| {
            let inner_lazy = local_def_impl(
                cx,
                &aloc,
                &file_and_dependency_map_rec,
                builtins.local_defs.get(*i),
            );
            let lazy_val: Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>> =
                Rc::new(Lazy::new(Box::new(move || {
                    //   (fun (loc, _, (lazy t), _) -> (loc, t))
                    let (loc, _, general, _) = Lazy::force(&*inner_lazy);
                    (loc.dupe(), Lazy::force(&*general.dupe()).dupe())
                })
                    as Box<dyn FnOnce() -> (ALoc, Type)>));
            (name.dupe(), lazy_val)
        })
        .collect();

    let builtin_types: BTreeMap<
        FlowSmolStr,
        Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>>,
    > = builtins
        .global_types
        .iter()
        .map(|(name, i)| {
            let inner_lazy = local_def_impl(
                cx,
                &aloc,
                &file_and_dependency_map_rec,
                builtins.local_defs.get(*i),
            );
            let lazy_val: Rc<Lazy<(ALoc, Type), Box<dyn FnOnce() -> (ALoc, Type)>>> =
                Rc::new(Lazy::new(Box::new(move || {
                    //   (fun (loc, _, (lazy t), _) -> (loc, t))
                    let (loc, _, general, _) = Lazy::force(&*inner_lazy);
                    (loc.dupe(), Lazy::force(&*general.dupe()).dupe())
                })
                    as Box<dyn FnOnce() -> (ALoc, Type)>));
            (name.dupe(), lazy_val)
        })
        .collect();

    let builtin_modules: BTreeMap<
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
    > = builtins
        .global_modules
        .keys()
        .map(|name| {
            let (_, dep_map) = Lazy::force(&*file_and_dependency_map_rec);
            let module = dep_map
                .get(name)
                .expect("module not found in dep_map")
                .dupe();
            (name.dupe(), module)
        })
        .collect();

    (builtin_values, builtin_types, builtin_modules)
}
