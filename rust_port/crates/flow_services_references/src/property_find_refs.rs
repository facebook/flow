/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocMap;
use flow_parser::ast;
use flow_parser::ast::expression;
use flow_parser::ast::expression::object;
use flow_parser::ast::pattern;
use flow_parser::ast::statement;
use flow_parser::ast_utils;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_parser::loc::Loc;
use flow_services_get_def::get_def_types::PropertyDefInfo;
use flow_services_get_def::get_def_types::SinglePropertyDefInfo;
use flow_services_get_def::get_def_utils;
use flow_services_get_def::object_key_visitor;
use flow_services_get_def::object_key_visitor::ObjectKeyVisitorCallback;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use vec1::Vec1;

use crate::find_refs_types::RefKind;
use crate::find_refs_types::SingleRef;

fn add_ref_kind(kind: RefKind, locs: Vec<Loc>) -> Vec<SingleRef> {
    locs.into_iter().map(|loc| (kind, loc)).collect()
}

mod literal_to_prop_loc {
    use super::*;

    struct LocMapBuilder {
        prop_name: String,
        acc: BTreeMap<Loc, Loc>,
    }

    impl ObjectKeyVisitorCallback for LocMapBuilder {
        fn visit_object_key(&mut self, literal_loc: &Loc, key: &object::Key<Loc, Loc>) {
            match key {
                object::Key::Identifier(id) if *id.name == *self.prop_name => {
                    self.acc.insert(literal_loc.dupe(), id.loc.dupe());
                }
                _ => {}
            }
        }
    }

    pub fn make(ast: &ast::Program<Loc, Loc>, prop_name: &str) -> BTreeMap<Loc, Loc> {
        let mut builder = LocMapBuilder {
            prop_name: prop_name.to_string(),
            acc: BTreeMap::new(),
        };
        object_key_visitor::visit(&mut builder, ast);
        builder.acc
    }
}

fn annot_of_jsx_name(name: &ast::jsx::Name<ALoc, (ALoc, Type)>) -> &(ALoc, Type) {
    match name {
        ast::jsx::Name::Identifier(ident) => &ident.loc,
        ast::jsx::Name::NamespacedName(nn) => &nn.name.loc,
        ast::jsx::Name::MemberExpression(me) => &me.property.loc,
    }
}

mod potential_ordinary_refs_search {
    use super::*;

    #[derive(Debug)]
    struct FoundImport(ALoc);

    #[derive(Debug)]
    enum SearcherError {
        Found(FoundImport),
        Job(flow_utils_concurrency::job_error::JobError),
    }

    impl From<FoundImport> for SearcherError {
        fn from(f: FoundImport) -> Self {
            SearcherError::Found(f)
        }
    }

    impl From<flow_utils_concurrency::job_error::JobError> for SearcherError {
        fn from(e: flow_utils_concurrency::job_error::JobError) -> Self {
            SearcherError::Job(e)
        }
    }

    struct Searcher<'ctx, 'cx, 'refs> {
        cx: &'ctx Context<'cx>,
        target_name: String,
        potential_refs: &'refs mut ALocMap<Type>,
    }

    impl<'ast> AstVisitor<'ast, ALoc, (ALoc, Type), &'ast ALoc, SearcherError>
        for Searcher<'_, '_, '_>
    {
        fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
            loc
        }

        fn normalize_type(type_: &'ast (ALoc, Type)) -> &'ast ALoc {
            &type_.0
        }

        fn import_declaration(
            &mut self,
            loc: &'ast ALoc,
            decl: &'ast statement::ImportDeclaration<ALoc, (ALoc, Type)>,
        ) -> Result<(), SearcherError> {
            match ast_visitor::import_declaration_default(self, loc, decl) {
                Ok(()) => Ok(()),
                Err(SearcherError::Found(FoundImport(name_loc))) => {
                    let (_, module_t) = &decl.source.0;
                    self.potential_refs.insert(name_loc, module_t.dupe());
                    Ok(())
                }
                Err(e @ SearcherError::Job(_)) => Err(e),
            }
        }

        fn import_named_specifier(
            &mut self,
            _import_kind: ast::statement::ImportKind,
            specifier: &'ast statement::import_declaration::NamedSpecifier<ALoc, (ALoc, Type)>,
        ) -> Result<(), SearcherError> {
            let id = specifier.local.as_ref().unwrap_or(&specifier.remote);
            let (name_loc, _) = &id.loc;
            let name = &id.name;
            if *name == *self.target_name {
                Err(SearcherError::Found(FoundImport(name_loc.dupe())))
            } else {
                Ok(())
            }
        }

        fn member(
            &mut self,
            loc: &'ast (ALoc, Type),
            expr: &'ast expression::Member<ALoc, (ALoc, Type)>,
        ) -> Result<(), SearcherError> {
            let (_, ty) = expr.object.loc();
            match &expr.property {
                expression::member::Property::PropertyIdentifier(id) => {
                    let (prop_loc, _) = &id.loc;
                    if *id.name == *self.target_name {
                        self.potential_refs.insert(prop_loc.dupe(), ty.dupe());
                    }
                }
                expression::member::Property::PropertyPrivateName(_)
                | expression::member::Property::PropertyExpression(_) => {}
            }
            ast_visitor::member_default(self, loc, expr)
        }

        fn jsx_opening_element(
            &mut self,
            elt: &'ast ast::jsx::Opening<ALoc, (ALoc, Type)>,
        ) -> Result<(), SearcherError> {
            let component_name = &elt.name;
            let attributes = &elt.attributes;
            for attr in attributes.iter() {
                match attr {
                    ast::jsx::OpeningAttribute::Attribute(jsx_attr)
                        if matches!(
                            &jsx_attr.name,
                            ast::jsx::attribute::Name::Identifier(id)
                            if *id.name == *self.target_name
                        ) =>
                    {
                        if let ast::jsx::attribute::Name::Identifier(id) = &jsx_attr.name {
                            let (attr_loc, _) = &id.loc;
                            let (_, component_t) = annot_of_jsx_name(component_name);
                            let reason = flow_common::reason::mk_reason(
                                flow_common::reason::VirtualReasonDesc::RReactProps,
                                attr_loc.dupe(),
                            );
                            use flow_typing_type::type_::*;
                            let props_object =
                                flow_typing_tvar::mk_where(self.cx, reason.dupe(), |_cx, tvar| {
                                    let use_op = UseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse));
                                    let use_t =
                                        UseT::new(UseTInner::ReactKitT(Box::new(ReactKitTData {
                                            use_op,
                                            reason: reason.dupe(),
                                            tool: Box::new(react::Tool::GetConfig {
                                                tout: tvar.dupe(),
                                            }),
                                        })));
                                    flow_typing_flow_js::flow_js::flow_non_speculating(
                                        self.cx,
                                        (component_t, &use_t),
                                    )
                                })?;
                            self.potential_refs.insert(attr_loc.dupe(), props_object);
                        }
                    }
                    _ => {}
                }
            }
            ast_visitor::jsx_opening_element_default(self, elt)
        }

        fn pattern(
            &mut self,
            kind: Option<ast::VariableKind>,
            expr: &'ast pattern::Pattern<ALoc, (ALoc, Type)>,
        ) -> Result<(), SearcherError> {
            let (unwrapped, _) = ast_utils::unwrap_nonnull_lhs(expr);
            let (_, ty) = unwrapped.loc();
            match &*unwrapped {
                pattern::Pattern::Object { inner, .. } => {
                    for prop in inner.properties.iter() {
                        match prop {
                            pattern::object::Property::NormalProperty(normal_prop) => {
                                match &normal_prop.key {
                                    pattern::object::Key::Identifier(id) => {
                                        let (prop_loc, _) = &id.loc;
                                        if *id.name == *self.target_name {
                                            self.potential_refs.insert(prop_loc.dupe(), ty.dupe());
                                        }
                                    }
                                    pattern::object::Key::StringLiteral((str_loc, str_lit)) => {
                                        if *str_lit.value == *self.target_name {
                                            self.potential_refs.insert(str_loc.dupe(), ty.dupe());
                                        }
                                    }
                                    pattern::object::Key::Computed(_)
                                    | pattern::object::Key::NumberLiteral(_)
                                    | pattern::object::Key::BigIntLiteral(_) => {}
                                }
                            }
                            pattern::object::Property::RestElement(_) => {}
                        }
                    }
                }
                pattern::Pattern::Identifier { inner, .. } => {
                    let (loc, ty) = &inner.name.loc;
                    let id_name = &inner.name.name;
                    if *id_name == *self.target_name && !self.potential_refs.contains_key(loc) {
                        self.potential_refs.insert(loc.dupe(), ty.dupe());
                    }
                }
                pattern::Pattern::Array { .. } | pattern::Pattern::Expression { .. } => {}
            }
            ast_visitor::pattern_default(self, kind, expr)
        }
    }

    pub fn search<'cx>(
        cx: &Context<'cx>,
        target_name: &str,
        potential_refs: &mut ALocMap<Type>,
        ast: &ast::Program<ALoc, (ALoc, Type)>,
    ) -> Result<(), flow_utils_concurrency::job_error::JobError> {
        let mut s = Searcher {
            cx,
            target_name: target_name.to_string(),
            potential_refs,
        };
        match s.program(ast) {
            Ok(()) => Ok(()),
            Err(SearcherError::Found(_)) => Ok(()),
            Err(SearcherError::Job(e)) => Err(e),
        }
    }
}

fn type_matches_locs<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    cx: &Context<'cx>,
    ty: &Type,
    prop_def_info: &Vec1<SinglePropertyDefInfo>,
    name: &str,
) -> Result<bool, String> {
    fn def_loc_matches_locs(
        def_loc: &get_def_utils::DefLoc,
        prop_def_info: &Vec1<SinglePropertyDefInfo>,
    ) -> bool {
        match def_loc {
            get_def_utils::DefLoc::FoundClass(ty_def_locs) => {
                prop_def_info.iter().any(|info| match info {
                    SinglePropertyDefInfo::ObjectProperty(_) => false,
                    SinglePropertyDefInfo::ClassProperty(loc) => *loc == *ty_def_locs.first(),
                })
            }
            get_def_utils::DefLoc::FoundObject(loc) => {
                prop_def_info.iter().any(|info| match info {
                    SinglePropertyDefInfo::ClassProperty(_) => false,
                    SinglePropertyDefInfo::ObjectProperty(def_loc) => *loc == *def_loc,
                })
            }
            get_def_utils::DefLoc::FoundUnion(def_locs) => def_locs
                .iter()
                .any(|dl| def_loc_matches_locs(dl, prop_def_info)),
            get_def_utils::DefLoc::NoDefFound
            | get_def_utils::DefLoc::UnsupportedType
            | get_def_utils::DefLoc::AnyType => false,
        }
    }
    get_def_utils::extract_def_loc(loc_of_aloc, cx, ty, name)
        .map(|dl| def_loc_matches_locs(&dl, prop_def_info))
}

fn get_loc_of_def_info<'cx>(
    cx: &Context<'cx>,
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    obj_to_obj_map: &BTreeMap<Loc, BTreeSet<flow_typing_type::type_::properties::Id>>,
    prop_def_info: &Vec1<SinglePropertyDefInfo>,
) -> Vec<Loc> {
    let mut prop_obj_locs: BTreeSet<Loc> = BTreeSet::new();
    for def_info in prop_def_info.iter() {
        match def_info {
            SinglePropertyDefInfo::ClassProperty(_) => {}
            SinglePropertyDefInfo::ObjectProperty(def_loc) => {
                prop_obj_locs.insert(def_loc.dupe());
            }
        }
    }
    let mut result: Vec<Loc> = Vec::new();
    for (loc, props_tmap_set) in obj_to_obj_map {
        for props_id in props_tmap_set {
            let props = cx.find_props(props_id.dupe());
            for (_name, prop) in props.iter() {
                if let Some(aloc) = flow_typing_type::type_::property::read_loc(prop) {
                    if prop_obj_locs.contains(&loc_of_aloc(&aloc)) {
                        result.push(loc.dupe());
                    }
                }
            }
        }
    }
    result
}

fn process_prop_refs<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    cx: &Context<'cx>,
    potential_refs: &ALocMap<Type>,
    file_key: &flow_parser::file_key::FileKey,
    prop_def_info: &Vec1<SinglePropertyDefInfo>,
    name: &str,
) -> Result<Vec<SingleRef>, String> {
    let results: Result<Vec<Option<Loc>>, String> = potential_refs
        .iter()
        .map(|(ref_loc, ty)| {
            type_matches_locs(loc_of_aloc, cx, ty, prop_def_info, name).map(|matches| {
                if matches {
                    Some(loc_of_aloc(ref_loc))
                } else {
                    None
                }
            })
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| {
            format!(
                "Encountered while finding refs in `{}`: {}",
                file_key.as_str(),
                err
            )
        });
    results.map(|refs| {
        add_ref_kind(
            RefKind::PropertyAccess,
            refs.into_iter().flatten().collect(),
        )
    })
}

fn ordinary_property_find_refs_in_file<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    ast_info: &flow_services_get_def::find_refs_utils::AstInfo,
    cx: &Context<'cx>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    obj_to_obj_map: &BTreeMap<Loc, BTreeSet<flow_typing_type::type_::properties::Id>>,
    file_key: &flow_parser::file_key::FileKey,
    props_info: &Vec1<SinglePropertyDefInfo>,
    name: &str,
) -> Result<Result<Vec<SingleRef>, String>, flow_utils_concurrency::job_error::JobError> {
    let mut potential_refs: ALocMap<Type> = ALocMap::new();
    let (ast, _file_sig, _info) = ast_info;
    let local_defs: Vec<SingleRef> = add_ref_kind(
        RefKind::PropertyDefinition,
        get_def_utils::all_locs_of_ordinary_property_def_info(props_info)
            .into_iter()
            .filter(|loc| loc.source.as_ref() == Some(file_key))
            .collect(),
    );
    potential_ordinary_refs_search::search(cx, name, &mut potential_refs, typed_ast)?;
    let prop_loc_map = once_cell::unsync::Lazy::new(|| literal_to_prop_loc::make(ast, name));
    let literal_prop_refs_result: Vec<SingleRef> = add_ref_kind(
        RefKind::PropertyDefinition,
        get_loc_of_def_info(cx, loc_of_aloc, obj_to_obj_map, props_info)
            .into_iter()
            .filter_map(|obj_loc| prop_loc_map.get(&obj_loc).cloned())
            .collect(),
    );
    Ok(
        process_prop_refs(loc_of_aloc, cx, &potential_refs, file_key, props_info, name).map(
            |mut refs| {
                let mut result = local_defs;
                result.append(&mut refs);
                let mut lit_refs = literal_prop_refs_result;
                result.append(&mut lit_refs);
                result
            },
        ),
    )
}

pub fn property_find_refs_in_file<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    ast_info: &flow_services_get_def::find_refs_utils::AstInfo,
    cx: &Context<'cx>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    obj_to_obj_map: &BTreeMap<Loc, BTreeSet<flow_typing_type::type_::properties::Id>>,
    file_key: &flow_parser::file_key::FileKey,
    prop_def_info: &PropertyDefInfo,
) -> Result<Result<Vec<SingleRef>, String>, flow_utils_concurrency::job_error::JobError> {
    match prop_def_info {
        PropertyDefInfo::OrdinaryProperty { props_info, name } => {
            ordinary_property_find_refs_in_file(
                loc_of_aloc,
                ast_info,
                cx,
                typed_ast,
                obj_to_obj_map,
                file_key,
                props_info,
                name,
            )
        }
        PropertyDefInfo::PrivateNameProperty {
            def_loc,
            references,
            name: _,
        } => {
            let mut result = vec![(RefKind::PropertyDefinition, def_loc.dupe())];
            for l in references {
                result.push((RefKind::PropertyAccess, l.dupe()));
            }
            Ok(Ok(result))
        }
    }
}

pub fn find_local_refs<'cx>(
    loc_of_aloc: &dyn Fn(&ALoc) -> Loc,
    file_key: &flow_parser::file_key::FileKey,
    ast_info: &flow_services_get_def::find_refs_utils::AstInfo,
    cx: &Context<'cx>,
    typed_ast: &ast::Program<ALoc, (ALoc, Type)>,
    obj_to_obj_map: &BTreeMap<Loc, BTreeSet<flow_typing_type::type_::properties::Id>>,
    loc: &Loc,
) -> Result<Result<Option<Vec<SingleRef>>, String>, flow_utils_concurrency::job_error::JobError> {
    let info_result =
        get_def_utils::get_property_def_info(loc_of_aloc, cx, typed_ast, obj_to_obj_map, loc);
    match info_result {
        Err(e) => Ok(Err(e)),
        Ok(None) => Ok(Ok(None)),
        Ok(Some(props_info)) => {
            let refs = property_find_refs_in_file(
                loc_of_aloc,
                ast_info,
                cx,
                typed_ast,
                obj_to_obj_map,
                file_key,
                &props_info,
            )?;
            match refs {
                Ok(refs) => Ok(Ok(Some(refs))),
                Err(e) => Ok(Err(e)),
            }
        }
    }
}
