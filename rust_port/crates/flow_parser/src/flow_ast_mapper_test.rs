/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use flow_data_structure_wrapper::smol_str::FlowSmolStr;

    use crate::ast;
    use crate::ast::statement;
    use crate::ast::statement::import_declaration;
    use crate::ast_visitor::AstVisitor;

    struct IdentityMapper;

    impl<'ast> AstVisitor<'ast, ()> for IdentityMapper {
        fn normalize_loc(loc: &'ast ()) -> &'ast () {
            loc
        }
        fn normalize_type(type_: &'ast ()) -> &'ast () {
            type_
        }
    }

    #[test]
    fn import_named_specifier() {
        let import_kind = statement::ImportKind::ImportValue;
        let source = (
            (),
            ast::StringLiteral {
                value: FlowSmolStr::from("foo"),
                raw: FlowSmolStr::from("\"foo\""),
                comments: None,
            },
        );
        let specifiers = vec![
            import_declaration::NamedSpecifier {
                kind: None,
                kind_loc: None,
                local: None,
                remote: ast::Identifier::new(ast::IdentifierInner {
                    loc: (),
                    name: FlowSmolStr::from("foo"),
                    comments: None,
                }),
                remote_name_def_loc: None,
            },
            import_declaration::NamedSpecifier {
                kind: None,
                kind_loc: None,
                local: Some(ast::Identifier::new(ast::IdentifierInner {
                    loc: (),
                    name: FlowSmolStr::from("baz"),
                    comments: None,
                })),
                remote: ast::Identifier::new(ast::IdentifierInner {
                    loc: (),
                    name: FlowSmolStr::from("bar"),
                    comments: None,
                }),
                remote_name_def_loc: None,
            },
        ];
        let ast = statement::Statement::new(statement::StatementInner::ImportDeclaration {
            loc: (),
            inner: Arc::new(statement::ImportDeclaration {
                import_kind,
                source,
                default: None,
                specifiers: Some(import_declaration::Specifier::ImportNamedSpecifiers(
                    specifiers,
                )),
                attributes: None,
                comments: None,
            }),
        });
        let mut mapper = IdentityMapper;
        let ast_prime = mapper.map_statement(&ast);
        assert_eq!(ast, ast_prime);
    }
}
