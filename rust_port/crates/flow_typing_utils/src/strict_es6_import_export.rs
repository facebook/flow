/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_aloc::ALocMap;
use flow_analysis::scope_api::ScopeInfo;
use flow_analysis::scope_builder;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::statement::StatementInner;
use flow_parser::ast_visitor;
use flow_parser::ast_visitor::AstVisitor;
use flow_typing_context::Context;
use flow_typing_context::Metadata;
use flow_typing_errors::error_message::EExportRenamedDefaultData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_flow_js::flow_js;

struct VarDeclInfo {
    decl_loc: ALoc,
    name: FlowSmolStr,
    kind: ast::VariableKind,
}

struct Declarations {
    /// Locs of ids and their import specifier for all `import *` declarations
    import_stars: ALocMap<(ALoc, ast::Identifier<ALoc, ALoc>)>,
    /// Locs of ids and information about the declaration for all variable declarations
    var_decls: ALocMap<VarDeclInfo>,
    /// Locs of ids and the function they are bound to
    functions: ALocMap<ast::function::Function<ALoc, ALoc>>,
    first_import: Option<ALoc>,
    first_require: Option<ALoc>,
}

fn declarations_init() -> Declarations {
    Declarations {
        import_stars: ALocMap::new(),
        var_decls: ALocMap::new(),
        functions: ALocMap::new(),
        first_import: None,
        first_require: None,
    }
}

fn is_require(expr: &ast::expression::Expression<ALoc, ALoc>) -> bool {
    match expr.deref() {
        ExpressionInner::Member { inner, .. } => is_require(&inner.object),
        ExpressionInner::Call { inner, .. } => {
            if let ExpressionInner::Identifier {
                inner: callee_id, ..
            } = inner.callee.deref()
            {
                if callee_id.name.as_str() == "require" {
                    if inner.arguments.arguments.len() == 1 {
                        if let ast::expression::ExpressionOrSpread::Expression(arg) =
                            &inner.arguments.arguments[0]
                        {
                            if let ExpressionInner::StringLiteral { .. } = arg.deref() {
                                return true;
                            }
                        }
                    }
                }
            }
            false
        }
        _ => false,
    }
}

fn has_value_import(import: &ast::statement::ImportDeclaration<ALoc, ALoc>) -> bool {
    match import {
        ast::statement::ImportDeclaration {
            import_kind: ast::statement::ImportKind::ImportValue,
            specifiers:
                Some(ast::statement::import_declaration::Specifier::ImportNamedSpecifiers(specifiers)),
            ..
        } => specifiers.iter().any(|spec| {
            spec.kind.is_none() || spec.kind == Some(ast::statement::ImportKind::ImportValue)
        }),
        ast::statement::ImportDeclaration {
            import_kind: ast::statement::ImportKind::ImportValue,
            ..
        } => true,
        _ => false,
    }
}

// Gather information about top level declarations to be used when checking for import/export errors.
fn gather_declarations(program: &ast::Program<ALoc, ALoc>) -> Declarations {
    fn add_import_star(acc: &mut Declarations, spec_loc: ALoc, id: ast::Identifier<ALoc, ALoc>) {
        let id_loc = id.loc.dupe();
        acc.import_stars.insert(id_loc, (spec_loc, id));
    }

    fn add_var_decl(
        acc: &mut Declarations,
        id_loc: ALoc,
        decl_loc: ALoc,
        name: FlowSmolStr,
        kind: ast::VariableKind,
    ) {
        acc.var_decls.insert(
            id_loc,
            VarDeclInfo {
                decl_loc,
                name,
                kind,
            },
        );
    }

    fn add_function(
        acc: &mut Declarations,
        id_loc: ALoc,
        func: ast::function::Function<ALoc, ALoc>,
    ) {
        acc.functions.insert(id_loc, func);
    }

    fn add_import(acc: &mut Declarations, import_loc: ALoc) {
        if acc.first_import.is_none() {
            acc.first_import = Some(import_loc);
        }
    }

    fn add_require(acc: &mut Declarations, require_loc: ALoc) {
        if acc.first_require.is_none() {
            acc.first_require = Some(require_loc);
        }
    }

    let statements = &program.statements;
    let mut acc = declarations_init();
    for stmt in statements.iter() {
        match stmt.deref() {
            StatementInner::VariableDeclaration { loc, inner } => {
                let kind = inner.kind;
                for declarator in inner.declarations.iter() {
                    let id = &declarator.id;
                    let init = &declarator.init;
                    // Gather all identifiers in variable declaration
                    flow_parser::ast_utils::fold_bindings_of_pattern(
                        (),
                        id,
                        &mut |(), identifier| {
                            add_var_decl(
                                &mut acc,
                                identifier.loc.dupe(),
                                loc.dupe(),
                                identifier.name.dupe(),
                                kind,
                            );
                        },
                    );
                    // Gather simple variable declarations where the init is a function, of the forms:
                    // const <ID> = function() { ... }
                    // const <ID> = () => { ... }
                    if let ast::pattern::Pattern::Identifier { inner: pat_id, .. } = id {
                        if let Some(init_expr) = init {
                            match init_expr.deref() {
                                ExpressionInner::ArrowFunction { inner: func, .. }
                                | ExpressionInner::Function { inner: func, .. } => {
                                    let id_loc = pat_id.name.loc.dupe();
                                    add_function(&mut acc, id_loc, func.as_ref().clone());
                                }
                                _ => {}
                            }
                        }
                    }
                    // Gather require loc if this is a require statement
                    if let Some(init_expr) = init {
                        if is_require(init_expr) {
                            add_require(&mut acc, loc.dupe());
                        }
                    }
                }
            }
            StatementInner::ImportDeclaration { loc, inner } => {
                // Gather import loc if this import statement imports a value
                if has_value_import(inner) {
                    add_import(&mut acc, loc.dupe());
                }
                match inner.as_ref() {
                    ast::statement::ImportDeclaration {
                        specifiers: Some(ast::statement::import_declaration::Specifier::ImportNamespaceSpecifier(specifier)),
                        import_kind: ast::statement::ImportKind::ImportValue,
                        ..
                    } => {
                        let (spec_loc, id) = specifier;
                        add_import_star(&mut acc, spec_loc.dupe(), id.clone());
                    }
                    _ => {}
                }
            }
            StatementInner::FunctionDeclaration { inner, .. } => {
                if let Some(ref func_id) = inner.id {
                    let id_loc = func_id.loc.dupe();
                    add_function(&mut acc, id_loc, inner.as_ref().clone());
                }
            }
            _ => {}
        }
    }
    acc
}

// Visitor that finds locs for each usage of `this` outside a class.
struct ThisVisitor {
    locs: Vec<ALoc>,
}

impl<'ast> AstVisitor<'ast, ALoc, ALoc, &'ast ALoc, !> for ThisVisitor {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast ALoc) -> &'ast ALoc {
        type_
    }

    fn this_expression(
        &mut self,
        loc: &'ast ALoc,
        _expr: &'ast ast::expression::This<ALoc>,
    ) -> Result<(), !> {
        self.locs.push(loc.dupe());
        Ok(())
    }

    // `this` is allowed in classes so do not recurse into class body
    fn class_body(&mut self, _cls_body: &'ast ast::class::Body<ALoc, ALoc>) -> Result<(), !> {
        // Do not recurse
        Ok(())
    }

    // Function decls and exprs will have a new scope, so `this` usage is allowed
    fn function_declaration(
        &mut self,
        _loc: &'ast ALoc,
        _stmt: &'ast ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), !> {
        // Do not recurse
        Ok(())
    }

    fn function_expression_or_method(
        &mut self,
        _loc: &'ast ALoc,
        _stmt: &'ast ast::function::Function<ALoc, ALoc>,
    ) -> Result<(), !> {
        Ok(())
    }
}

// Visitor that uses the previously found declaration info to check for errors in imports/exports.
struct ImportExportVisitor<'cx, 'a> {
    cx: &'a Context<'cx>,
    scope_info: &'a ScopeInfo<ALoc>,
    declarations: &'a Declarations,
    import_star_uses: ALocMap<(ALoc, ast::Identifier<ALoc, ALoc>)>,
}

impl<'cx, 'a> ImportExportVisitor<'cx, 'a> {
    fn new(
        cx: &'a Context<'cx>,
        scope_info: &'a ScopeInfo<ALoc>,
        declarations: &'a Declarations,
    ) -> Self {
        // Create a map from import star use locs to the import star specifier
        let mut import_star_uses = ALocMap::new();
        for (def_loc, specifier) in &declarations.import_stars {
            let def = scope_info.def_of_use_opt(def_loc).expect("def_of_use");
            let uses = scope_info.uses_of_def(def, false);
            for use_loc in uses.iter() {
                import_star_uses.insert(use_loc.dupe(), specifier.clone());
            }
        }
        ImportExportVisitor {
            cx,
            scope_info,
            declarations,
            import_star_uses,
        }
    }

    fn add_error(&self, err: ErrorMessage<ALoc>) {
        flow_js::add_output_non_speculating(self.cx, err)
    }

    fn import_star_reason(
        &self,
        import_star: &(ALoc, ast::Identifier<ALoc, ALoc>),
    ) -> VirtualReason<ALoc> {
        let (import_star_loc, _) = import_star;
        mk_reason(
            VirtualReasonDesc::RCode("import *".into()),
            import_star_loc.dupe(),
        )
    }

    fn add_bad_default_import_access_error(
        &self,
        loc: ALoc,
        import_star: &(ALoc, ast::Identifier<ALoc, ALoc>),
    ) {
        let import_star_reason = self.import_star_reason(import_star);
        self.add_error(ErrorMessage::EBadDefaultImportAccess(Box::new((
            loc,
            import_star_reason,
        ))))
    }

    fn add_bad_default_import_destructuring_error(&self, loc: ALoc) {
        self.add_error(ErrorMessage::EBadDefaultImportDestructuring(loc))
    }

    fn add_invalid_import_star_use_error(
        &self,
        loc: ALoc,
        import_star: &(ALoc, ast::Identifier<ALoc, ALoc>),
    ) {
        let import_star_reason = self.import_star_reason(import_star);
        self.add_error(ErrorMessage::EInvalidImportStarUse(Box::new((
            loc,
            import_star_reason,
        ))))
    }

    fn add_non_const_var_export_error(
        &self,
        loc: ALoc,
        decl_info: Option<(ALoc, flow_common::reason::Name)>,
    ) {
        let decl_reason = decl_info
            .map(|(decl_loc, name)| mk_reason(VirtualReasonDesc::RIdentifier(name), decl_loc));
        self.add_error(ErrorMessage::ENonConstVarExport(Box::new((
            loc,
            decl_reason,
        ))))
    }

    fn add_this_in_exported_function_error(&self, loc: ALoc) {
        self.add_error(ErrorMessage::EThisInExportedFunction(loc))
    }

    fn add_export_named_default_error(
        &self,
        loc: ALoc,
        name: Option<flow_data_structure_wrapper::smol_str::FlowSmolStr>,
        is_reexport: bool,
    ) {
        self.add_error(ErrorMessage::EExportRenamedDefault(Box::new(
            EExportRenamedDefaultData {
                loc,
                name,
                is_reexport,
            },
        )))
    }

    fn import_star_from_use(&self, use_loc: &ALoc) -> Option<&(ALoc, ast::Identifier<ALoc, ALoc>)> {
        self.import_star_uses.get(use_loc)
    }

    fn is_import_star_use(&self, use_loc: &ALoc) -> bool {
        self.import_star_from_use(use_loc).is_some()
    }

    fn add_exported_this_errors(&self, func: &ast::function::Function<ALoc, ALoc>) {
        let params = &func.params;
        match &params.this_ {
            // if the function has a this parameter, we don't want to report any errors because the user has
            // told us what this points at.
            Some(_) => (),
            None => {
                let mut this_visitor = ThisVisitor { locs: Vec::new() };
                let Ok(()) = this_visitor.function_body_any(&func.body);
                for loc in this_visitor.locs {
                    self.add_this_in_exported_function_error(loc);
                }
            }
        }
    }

    fn object_pattern_default_property(
        &self,
        object_pattern: &ast::pattern::Object<ALoc, ALoc>,
    ) -> Option<ALoc> {
        let properties = &object_pattern.properties;
        properties.iter().find_map(|prop| match prop {
            ast::pattern::object::Property::NormalProperty(
                ast::pattern::object::NormalProperty { key, .. },
            ) => match key {
                ast::pattern::object::Key::Identifier(id) if id.name.as_str() == "default" => {
                    Some(id.loc.dupe())
                }
                ast::pattern::object::Key::StringLiteral((default_loc, lit))
                    if lit.value.as_str() == "default" =>
                {
                    Some(default_loc.dupe())
                }
                _ => None,
            },
            _ => None,
        })
    }

    fn is_simple_object_destructuring(
        &self,
        object_pattern: &ast::pattern::Object<ALoc, ALoc>,
    ) -> bool {
        let properties = &object_pattern.properties;
        properties.iter().all(|prop| {
            matches!(
                prop,
                ast::pattern::object::Property::NormalProperty(
                    ast::pattern::object::NormalProperty {
                        key: ast::pattern::object::Key::Identifier(_)
                            | ast::pattern::object::Key::StringLiteral(_)
                            | ast::pattern::object::Key::NumberLiteral(_)
                            | ast::pattern::object::Key::BigIntLiteral(_),
                        ..
                    }
                )
            )
        })
    }
}

impl<'cx, 'a, 'ast> AstVisitor<'ast, ALoc, ALoc, &'ast ALoc, !> for ImportExportVisitor<'cx, 'a> {
    fn normalize_loc(loc: &'ast ALoc) -> &'ast ALoc {
        loc
    }

    fn normalize_type(type_: &'ast ALoc) -> &'ast ALoc {
        type_
    }

    fn expression(&mut self, expr: &'ast ast::expression::Expression<ALoc, ALoc>) -> Result<(), !> {
        match expr.deref() {
            // Error on use of module object. Valid use of module object will not recurse to this point.
            ExpressionInner::Identifier { inner: id, .. } => {
                let id_loc = &id.loc;
                match self.import_star_from_use(id_loc) {
                    Some(import_star) => {
                        let import_star = import_star.clone();
                        self.add_invalid_import_star_use_error(id_loc.dupe(), &import_star);
                        Ok(())
                    }
                    None => ast_visitor::expression_default(self, expr),
                }
            }
            _ => ast_visitor::expression_default(self, expr),
        }
    }

    fn member(
        &mut self,
        loc: &'ast ALoc,
        expr: &'ast ast::expression::Member<ALoc, ALoc>,
    ) -> Result<(), !> {
        if let ExpressionInner::Identifier { inner: id, .. } = expr.object.deref() {
            let id_loc = &id.loc;
            let import_star = self.import_star_from_use(id_loc).cloned();
            let property = &expr.property;
            match (&import_star, property) {
                // Error on attempt to access default export
                (
                    Some(import_star),
                    ast::expression::member::Property::PropertyIdentifier(prop_id),
                ) if prop_id.name.as_str() == "default" => {
                    self.add_bad_default_import_access_error(loc.dupe(), import_star);
                    return Ok(());
                }
                (
                    Some(import_star),
                    ast::expression::member::Property::PropertyExpression(prop_expr),
                ) if matches!(
                    prop_expr.deref(),
                    ExpressionInner::StringLiteral { inner, .. } if inner.value.as_str() == "default"
                ) =>
                {
                    self.add_bad_default_import_access_error(loc.dupe(), import_star);
                    return Ok(());
                }
                // Do not recurse on valid use of module object
                (Some(_), ast::expression::member::Property::PropertyIdentifier(_)) => {
                    return Ok(());
                }
                (Some(_), ast::expression::member::Property::PropertyExpression(prop_expr))
                    if matches!(prop_expr.deref(), ExpressionInner::StringLiteral { .. }) =>
                {
                    return Ok(());
                }
                _ => {}
            }
        }
        ast_visitor::member_default(self, loc, expr)
    }

    fn variable_declarator(
        &mut self,
        kind: ast::VariableKind,
        declarator: &'ast ast::statement::variable::Declarator<ALoc, ALoc>,
    ) -> Result<(), !> {
        let id = &declarator.id;
        let init = &declarator.init;
        if let ast::pattern::Pattern::Object {
            inner: object_pattern,
            ..
        } = id
        {
            if let Some(init_expr) = init {
                if let ExpressionInner::Identifier { inner: init_id, .. } = init_expr.deref() {
                    let id_loc = &init_id.loc;
                    match self.import_star_from_use(id_loc) {
                        Some(import_star) => {
                            let import_star = import_star.clone();
                            // Error on attempt to access default export
                            if let Some(default_loc) =
                                self.object_pattern_default_property(object_pattern)
                            {
                                self.add_bad_default_import_access_error(default_loc, &import_star)
                            }
                            if self.is_simple_object_destructuring(object_pattern) {
                                self.variable_declarator_pattern(kind, id);
                                return Ok(());
                            } else {
                                return ast_visitor::variable_declarator_default(
                                    self, kind, declarator,
                                );
                            }
                        }
                        None => {
                            return ast_visitor::variable_declarator_default(
                                self, kind, declarator,
                            );
                        }
                    }
                }
            }
        }
        ast_visitor::variable_declarator_default(self, kind, declarator)
    }

    fn assignment(
        &mut self,
        loc: &'ast ALoc,
        assign: &'ast ast::expression::Assignment<ALoc, ALoc>,
    ) -> Result<(), !> {
        if let ast::pattern::Pattern::Object {
            inner: object_pattern,
            ..
        } = &assign.left
        {
            if let ExpressionInner::Identifier {
                inner: right_id, ..
            } = assign.right.deref()
            {
                let id_loc = &right_id.loc;
                let default_loc = self.object_pattern_default_property(object_pattern);
                let import_star = self.import_star_from_use(id_loc).cloned();
                match (default_loc, &import_star) {
                    // Error on attempt to access default export
                    (Some(default_loc), Some(import_star)) => {
                        self.add_bad_default_import_access_error(default_loc, import_star);
                        return Ok(());
                    }
                    // Do not recurse since RHS is a valid use of module object
                    (None, Some(_)) if self.is_simple_object_destructuring(object_pattern) => {
                        return Ok(());
                    }
                    _ => {}
                }
            }
        }
        ast_visitor::assignment_default(self, loc, assign)
    }

    fn import_declaration(
        &mut self,
        loc: &'ast ALoc,
        decl: &'ast ast::statement::ImportDeclaration<ALoc, ALoc>,
    ) -> Result<(), !> {
        if let Some(ast::statement::import_declaration::Specifier::ImportNamedSpecifiers(
            specifiers,
        )) = &decl.specifiers
        {
            for specifier in specifiers {
                if specifier.remote.name.as_str() == "default" {
                    let default_loc = specifier.remote.loc.dupe();
                    self.add_bad_default_import_destructuring_error(default_loc);
                }
            }
        }
        ast_visitor::import_declaration_default(self, loc, decl)
    }

    fn typeof_expression(
        &mut self,
        git: &'ast ast::types::typeof_::Target<ALoc, ALoc>,
    ) -> Result<(), !> {
        match git {
            // Error on unqualified use of module object
            ast::types::typeof_::Target::Unqualified(id) => {
                let id_loc = &id.loc;
                match self.import_star_from_use(id_loc) {
                    Some(import_star) => {
                        let import_star = import_star.clone();
                        self.add_invalid_import_star_use_error(id_loc.dupe(), &import_star);
                        Ok(())
                    }
                    None => ast_visitor::typeof_expression_default(self, git),
                }
            }
            // Do not recurse on valid use of module object
            ast::types::typeof_::Target::Qualified(qual) => {
                if let ast::types::typeof_::Target::Unqualified(id) = &qual.qualification {
                    if self.is_import_star_use(&id.loc) {
                        return Ok(());
                    }
                }
                ast_visitor::typeof_expression_default(self, git)
            }
            ast::types::typeof_::Target::Import(_) => {
                ast_visitor::typeof_expression_default(self, git)
            }
        }
    }

    fn generic_identifier_type(
        &mut self,
        git: &'ast ast::types::generic::Identifier<ALoc, ALoc>,
    ) -> Result<(), !> {
        match git {
            // Error on unqualified use of module object
            ast::types::generic::Identifier::Unqualified(id) => {
                let id_loc = &id.loc;
                match self.import_star_from_use(id_loc) {
                    Some(import_star) => {
                        let import_star = import_star.clone();
                        self.add_invalid_import_star_use_error(id_loc.dupe(), &import_star);
                        Ok(())
                    }
                    None => ast_visitor::generic_identifier_type_default(self, git),
                }
            }
            // Do not recurse on valid use of module object
            ast::types::generic::Identifier::Qualified(qual) => {
                if let ast::types::generic::Identifier::Unqualified(id) = &qual.qualification {
                    if self.is_import_star_use(&id.loc) {
                        return Ok(());
                    }
                }
                ast_visitor::generic_identifier_type_default(self, git)
            }
            _ => ast_visitor::generic_identifier_type_default(self, git),
        }
    }

    fn jsx_element(
        &mut self,
        loc: &'ast ALoc,
        elem: &'ast ast::jsx::Element<ALoc, ALoc>,
    ) -> Result<(), !> {
        let name = &elem.opening_element.name;
        // Error on use of module object outside member expression
        if let ast::jsx::Name::Identifier(jsx_id) = name {
            let id_loc = &jsx_id.loc;
            if self.is_import_star_use(id_loc) {
                if let Some(import_star) = self.import_star_from_use(id_loc) {
                    let import_star = import_star.clone();
                    self.add_invalid_import_star_use_error(id_loc.dupe(), &import_star);
                }
            }
        }
        ast_visitor::jsx_element_default(self, loc, elem)
    }

    fn export_named_declaration(
        &mut self,
        loc: &'ast ALoc,
        decl: &'ast ast::statement::ExportNamedDeclaration<ALoc, ALoc>,
    ) -> Result<(), !> {
        let declaration = &decl.declaration;
        let specifiers = &decl.specifiers;
        let source = &decl.source;

        // Only const variables can be exported
        if let Some(stmt) = declaration {
            if let StatementInner::VariableDeclaration {
                loc: var_loc,
                inner,
            } = stmt.deref()
            {
                match inner.kind {
                    ast::VariableKind::Var | ast::VariableKind::Let => {
                        self.add_non_const_var_export_error(var_loc.dupe(), None);
                    }
                    _ => {}
                }
            }
        }

        // Check for usage of this in exported functions
        if let Some(stmt) = declaration {
            match stmt.deref() {
                StatementInner::FunctionDeclaration { inner: func, .. } => {
                    self.add_exported_this_errors(func);
                }
                StatementInner::VariableDeclaration { inner, .. } => {
                    for declarator in inner.declarations.iter() {
                        if let Some(init) = &declarator.init {
                            match init.deref() {
                                ExpressionInner::ArrowFunction { inner: func, .. }
                                | ExpressionInner::Function { inner: func, .. } => {
                                    self.add_exported_this_errors(func);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        if let Some(ast::statement::export_named_declaration::Specifier::ExportSpecifiers(
            specifiers,
        )) = specifiers
        {
            for specifier in specifiers {
                let spec_loc = &specifier.loc;
                let local = &specifier.local;
                let id_loc = &local.loc;
                let name = &local.name;
                let exported = &specifier.exported;

                // Check for renaming export to be default export
                match exported {
                    Some(exported_id) if exported_id.name.as_str() == "default" => {
                        self.add_export_named_default_error(
                            spec_loc.dupe(),
                            Some(name.dupe()),
                            source.is_some(),
                        );
                    }
                    None if name.as_str() == "default" && source.is_some() => {
                        self.add_export_named_default_error(spec_loc.dupe(), None, true);
                    }
                    _ => {}
                }
                if let Some(def) = self.scope_info.def_of_use_opt(id_loc) {
                    let def_loc = def.locs.first().dupe();
                    // Also check for non-const variables in list of export specifiers
                    if let Some(var_decl) = self.declarations.var_decls.get(&def_loc) {
                        match var_decl.kind {
                            ast::VariableKind::Var | ast::VariableKind::Let => {
                                self.add_non_const_var_export_error(
                                    id_loc.dupe(),
                                    Some((
                                        var_decl.decl_loc.dupe(),
                                        flow_common::reason::Name::new(
                                            flow_data_structure_wrapper::smol_str::FlowSmolStr::new(
                                                &var_decl.name,
                                            ),
                                        ),
                                    )),
                                );
                            }
                            _ => {}
                        }
                    }
                    // Check for `this` if exported variable is bound to a function
                    if let Some(func) = self.declarations.functions.get(&def_loc) {
                        self.add_exported_this_errors(func);
                    }
                }
            }
        }
        ast_visitor::export_named_declaration_default(self, loc, decl)
    }

    fn export_default_declaration(
        &mut self,
        loc: &'ast ALoc,
        decl: &'ast ast::statement::ExportDefaultDeclaration<ALoc, ALoc>,
    ) -> Result<(), !> {
        let declaration = &decl.declaration;
        match declaration {
            ast::statement::export_default_declaration::Declaration::Declaration(stmt) => {
                if let StatementInner::FunctionDeclaration { inner: func, .. } = stmt.deref() {
                    self.add_exported_this_errors(func);
                }
            }
            ast::statement::export_default_declaration::Declaration::Expression(expr) => {
                match expr.deref() {
                    ExpressionInner::ArrowFunction { inner: func, .. }
                    | ExpressionInner::Function { inner: func, .. } => {
                        self.add_exported_this_errors(func);
                    }
                    _ => {}
                }
            }
        }
        ast_visitor::export_default_declaration_default(self, loc, decl)
    }
}

fn detect_mixed_import_and_require_error<'cx>(cx: &Context<'cx>, declarations: &Declarations) {
    match (&declarations.first_import, &declarations.first_require) {
        (Some(first_import_loc), Some(first_require_loc)) => {
            let import_reason = mk_reason(
                VirtualReasonDesc::RCode("import".into()),
                first_import_loc.dupe(),
            );
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EMixedImportAndRequire(Box::new((
                    first_require_loc.dupe(),
                    import_reason,
                ))),
            )
        }
        _ => (),
    }
}

fn detect_errors_from_ast<'cx>(cx: &Context<'cx>, ast: &ast::Program<ALoc, ALoc>) {
    let scope_info = scope_builder::program(cx.enable_enums(), true, ast);
    let declarations = gather_declarations(ast);
    detect_mixed_import_and_require_error(cx, &declarations);
    let mut visitor = ImportExportVisitor::new(cx, &scope_info, &declarations);
    let Ok(()) = visitor.program(ast);
}

pub fn detect_errors<'cx>(cx: &Context<'cx>, ast: &ast::Program<ALoc, ALoc>, metadata: &Metadata) {
    if metadata.frozen.strict_es6_import_export {
        detect_errors_from_ast(cx, ast)
    }
}
