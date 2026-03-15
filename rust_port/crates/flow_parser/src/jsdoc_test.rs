/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::ast;
    use crate::jsdoc;
    use crate::jsdoc::param;

    // ****************************
    // * AST construction helpers *
    // ****************************

    fn mk_line_comment(text: &str) -> Option<ast::Syntax<(), ()>> {
        Some(ast::Syntax {
            leading: Arc::from([ast::Comment {
                loc: (),
                kind: ast::CommentKind::Line,
                text: Arc::from(text),
                on_newline: false,
            }]),
            trailing: Arc::from([]),
            internal: (),
        })
    }

    fn mk_block_comment(text: &str) -> Option<ast::Syntax<(), ()>> {
        Some(ast::Syntax {
            leading: Arc::from([ast::Comment {
                loc: (),
                kind: ast::CommentKind::Block,
                text: Arc::from(text),
                on_newline: false,
            }]),
            trailing: Arc::from([]),
            internal: (),
        })
    }

    fn mk_block_comments(leading: &[&str], trailing: &[&str]) -> Option<ast::Syntax<(), ()>> {
        let block = |text: &&str| ast::Comment {
            loc: (),
            kind: ast::CommentKind::Block,
            text: Arc::from(*text),
            on_newline: false,
        };
        Some(ast::Syntax {
            leading: leading.iter().map(block).collect(),
            trailing: trailing.iter().map(block).collect(),
            internal: (),
        })
    }

    // ***********
    // * testing *
    // ***********

    fn string_of_option<T>(f: impl Fn(&T) -> String, opt: &Option<T>) -> String {
        match opt {
            None => "None".to_string(),
            Some(x) => format!("Some {}", f(x)),
        }
    }
    fn mk_test(
        comment: Option<ast::Syntax<(), ()>>,
        should_not_parse: bool,
        description: Option<Option<String>>,
        params: Option<Vec<(String, param::T)>>,
        unrecognized_tags: Option<Vec<(String, Option<String>)>>,
        deprecated: Option<String>,
    ) {
        match jsdoc::of_comments(comment.as_ref()) {
            None => assert!(should_not_parse, "JSDoc didn't parse"),
            Some((_, jsdoc)) => {
                if let Some(ref description) = description {
                    assert_eq!(
                        description,
                        jsdoc.description(),
                        "description mismatch: expected {:?}, got {:?}",
                        string_of_option(|s: &String| format!("{:?}", s), description),
                        string_of_option(|s: &String| format!("{:?}", s), jsdoc.description()),
                    );
                }
                if let Some(ref params) = params {
                    assert_eq!(params, &jsdoc.params().0, "params mismatch",);
                }
                assert_eq!(
                    deprecated.as_ref(),
                    jsdoc.deprecated(),
                    "deprecated mismatch",
                );
                if let Some(ref unrecognized_tags) = unrecognized_tags {
                    assert_eq!(
                        unrecognized_tags,
                        &jsdoc.unrecognized_tags().0,
                        "unrecognized tags mismatch",
                    );
                }
            }
        }
    }

    #[test]
    fn dont_parse_line() {
        mk_test(mk_line_comment("* foo"), true, None, None, None, None);
    }

    #[test]
    fn parse_description() {
        mk_test(
            mk_block_comment("* foo"),
            false,
            Some(Some(" foo".to_string())),
            None,
            None,
            None,
        );
    }

    #[test]
    fn parse_deprecated_with_str() {
        mk_test(
            mk_block_comment("* @deprecated foo"),
            false,
            None,
            None,
            None,
            Some(" foo".to_string()),
        );
    }

    #[test]
    fn parse_deprecated_without_str() {
        mk_test(
            mk_block_comment("* @deprecated"),
            false,
            None,
            None,
            None,
            Some("".to_string()),
        );
    }

    #[test]
    fn trim_asterisks() {
        mk_test(
            mk_block_comment(
                r#"*
                    *   foo
                    *   bar
                    *   @snap crackle
                    *   "#,
            ),
            false,
            Some(Some("\n   foo\n   bar".to_string())),
            None,
            None,
            None,
        );
    }

    #[test]
    fn pick_last_jsdoc_containing_leading_comment() {
        mk_test(
            mk_block_comments(&["* foo", "bar", "* baz", "snap"], &["* crackle"]),
            false,
            Some(Some(" baz".to_string())),
            None,
            None,
            None,
        );
    }

    #[test]
    fn no_description() {
        mk_test(
            mk_block_comment("*\n @unsupported"),
            false,
            Some(None),
            None,
            None,
            None,
        );
    }

    #[test]
    fn simple_param_descriptions() {
        mk_test(
            mk_block_comment(
                r#"* overall description
                    * @param a description for a
                    * @param {ignore this} b description for b
                    * @param c - hyphen before description for c
                    * @param d
                      @param
                      @param e  multiline
                    *           param description
                    * @unsupported tag to be skipped
                    * @arg f - arg alias for param tag
                    * @argument g - argument alias for param tag
                      @param
                      @param h
                      @param i description before eof"#,
            ),
            false,
            Some(Some(" overall description".to_string())),
            Some(vec![
                (
                    "a".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("description for a".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "b".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("description for b".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "c".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some(" hyphen before description for c".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "d".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: None,
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "e".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some(
                                "multiline\n           param description".to_string(),
                            ),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "f".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some(" arg alias for param tag".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "g".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some(" argument alias for param tag".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "h".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: None,
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "i".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("description before eof".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
            ]),
            None,
            None,
        );
    }

    #[test]
    fn advanced_params() {
        mk_test(
            mk_block_comment(
                r#"*
                    * @param [foo] - optional foo
                    * @param bar[] - element path of bar
                    * @param bar.x - member x path of bar
                    * @param [bar.y=this is a default] - optional member y path of bar with default
                    * @param [baz[]=this is another default] - optional element path of baz with default
                    * @param baz.bar.foo member foo of member bar of baz
                    * @param baz[].foo.bar member bar of member foo of element of baz
                    * @param qux.x.y[].z
                    * @param [qux[][].x]
                    * @param [qux=has a default]
                  "#,
            ),
            false,
            None,
            Some(vec![
                (
                    "foo".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some(" optional foo".to_string()),
                            optional: param::Optionality::Optional,
                        },
                    )],
                ),
                (
                    "bar".to_string(),
                    vec![
                        (
                            param::Path::Element(Box::new(param::Path::Name)),
                            param::Info {
                                description: Some(" element path of bar".to_string()),
                                optional: param::Optionality::NotOptional,
                            },
                        ),
                        (
                            param::Path::Member(Box::new(param::Path::Name), "x".to_string()),
                            param::Info {
                                description: Some(" member x path of bar".to_string()),
                                optional: param::Optionality::NotOptional,
                            },
                        ),
                        (
                            param::Path::Member(Box::new(param::Path::Name), "y".to_string()),
                            param::Info {
                                description: Some(
                                    " optional member y path of bar with default".to_string(),
                                ),
                                optional: param::Optionality::OptionalWithDefault(
                                    "this is a default".to_string(),
                                ),
                            },
                        ),
                    ],
                ),
                (
                    "baz".to_string(),
                    vec![
                        (
                            param::Path::Element(Box::new(param::Path::Name)),
                            param::Info {
                                description: Some(
                                    " optional element path of baz with default".to_string(),
                                ),
                                optional: param::Optionality::OptionalWithDefault(
                                    "this is another default".to_string(),
                                ),
                            },
                        ),
                        (
                            param::Path::Member(
                                Box::new(param::Path::Member(
                                    Box::new(param::Path::Name),
                                    "bar".to_string(),
                                )),
                                "foo".to_string(),
                            ),
                            param::Info {
                                description: Some("member foo of member bar of baz".to_string()),
                                optional: param::Optionality::NotOptional,
                            },
                        ),
                        (
                            param::Path::Member(
                                Box::new(param::Path::Member(
                                    Box::new(param::Path::Element(Box::new(param::Path::Name))),
                                    "foo".to_string(),
                                )),
                                "bar".to_string(),
                            ),
                            param::Info {
                                description: Some(
                                    "member bar of member foo of element of baz".to_string(),
                                ),
                                optional: param::Optionality::NotOptional,
                            },
                        ),
                    ],
                ),
                (
                    "qux".to_string(),
                    vec![
                        (
                            param::Path::Member(
                                Box::new(param::Path::Element(Box::new(param::Path::Member(
                                    Box::new(param::Path::Member(
                                        Box::new(param::Path::Name),
                                        "x".to_string(),
                                    )),
                                    "y".to_string(),
                                )))),
                                "z".to_string(),
                            ),
                            param::Info {
                                description: None,
                                optional: param::Optionality::NotOptional,
                            },
                        ),
                        (
                            param::Path::Member(
                                Box::new(param::Path::Element(Box::new(param::Path::Element(
                                    Box::new(param::Path::Name),
                                )))),
                                "x".to_string(),
                            ),
                            param::Info {
                                description: None,
                                optional: param::Optionality::Optional,
                            },
                        ),
                        (
                            param::Path::Name,
                            param::Info {
                                description: None,
                                optional: param::Optionality::OptionalWithDefault(
                                    "has a default".to_string(),
                                ),
                            },
                        ),
                    ],
                ),
            ]),
            None,
            None,
        );
    }

    #[test]
    fn description_tag() {
        mk_test(
            mk_block_comment(
                r#"* this description will be overridden
                    * @param a description for a
                    * @description this description overrides the first one
                    *   and can have multiple lines
                    * @param b another parameter"#,
            ),
            false,
            Some(Some(
                " this description overrides the first one\n   and can have multiple lines"
                    .to_string(),
            )),
            Some(vec![
                (
                    "a".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("description for a".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "b".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("another parameter".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
            ]),
            None,
            None,
        );
    }

    #[test]
    fn desc_tag() {
        mk_test(
            mk_block_comment(
                r#"* this description will be overridden
                    * @param a description for a
                    * @desc this desc overrides the first one
                    *   and can have multiple lines
                    * @param b another parameter"#,
            ),
            false,
            Some(Some(
                " this desc overrides the first one\n   and can have multiple lines".to_string(),
            )),
            Some(vec![
                (
                    "a".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("description for a".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
                (
                    "b".to_string(),
                    vec![(
                        param::Path::Name,
                        param::Info {
                            description: Some("another parameter".to_string()),
                            optional: param::Optionality::NotOptional,
                        },
                    )],
                ),
            ]),
            None,
            None,
        );
    }

    #[test]
    fn unrecognized_tags() {
        mk_test(
            mk_block_comment(
                r#"* description
                    * @explorer-desc
                    * contents of explorer-desc tag
                    * @explorer-ignore
                    * @explorer-title some title
                    * @desc description 2 "#,
            ),
            false,
            Some(Some(" description 2".to_string())),
            None,
            Some(vec![
                (
                    "explorer-desc".to_string(),
                    Some("\n contents of explorer-desc tag".to_string()),
                ),
                ("explorer-ignore".to_string(), None),
                (
                    "explorer-title".to_string(),
                    Some(" some title".to_string()),
                ),
            ]),
            None,
        );
    }
}
