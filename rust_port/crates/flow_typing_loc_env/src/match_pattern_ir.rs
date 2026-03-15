/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// =============================================================================
// OCaml: Match_pattern_ir from flow/src/typing/match_pattern_ir.ml
// =============================================================================

// open Loc_collections
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_aloc::ALoc;
use flow_aloc::ALocId;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_env_builder::selector::Selector;
use flow_parser::ast::match_pattern::MatchPattern;
use flow_parser::loc::Loc;
use flow_parser_utils_output::js_layout_generator;
use flow_typing_type::type_::BigIntLiteral;
use flow_typing_type::type_::EnumConcreteInfo;
use flow_typing_type::type_::NumberLiteral;
use flow_typing_type::type_::Type;

pub type PatternAstList = Vec<(MatchPattern<ALoc, (ALoc, Type)>, /* guarded */ bool)>;

/// Either a primitive literal or a Flow Enum member.
/// Used in both `PatternUnion`s and `ValueUnion`s.
pub mod leaf {
    use flow_parser::loc_sig::LocSig;

    use super::*;

    #[derive(Debug, Clone)]
    pub struct EnumMember {
        pub enum_info: Rc<EnumConcreteInfo>,
        pub member_name: FlowSmolStr,
    }

    impl PartialEq for EnumMember {
        fn eq(&self, other: &Self) -> bool {
            self.enum_info.enum_id == other.enum_info.enum_id
                && self.member_name == other.member_name
        }
    }

    impl Eq for EnumMember {}

    impl PartialOrd for EnumMember {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for EnumMember {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            let compare_id = self.enum_info.enum_id.cmp(&other.enum_info.enum_id);
            if compare_id == std::cmp::Ordering::Equal {
                self.member_name.cmp(&other.member_name)
            } else {
                compare_id
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub enum LeafCtor {
        BoolC(bool),
        StrC(Name),
        NumC(NumberLiteral),
        BigIntC(BigIntLiteral),
        NullC,
        VoidC,
        EnumMemberC(EnumMember),
    }

    impl std::fmt::Display for LeafCtor {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                LeafCtor::BoolC(true) => write!(f, "true"),
                LeafCtor::BoolC(false) => write!(f, "false"),
                LeafCtor::StrC(name) => {
                    write!(
                        f,
                        "{}",
                        js_layout_generator::quote_string(true, name.as_str())
                    )
                }
                LeafCtor::NumC(NumberLiteral(_, s)) => write!(f, "{}", s),
                LeafCtor::BigIntC(BigIntLiteral(_, s)) => write!(f, "{}", s),
                LeafCtor::NullC => write!(f, "null"),
                LeafCtor::VoidC => write!(f, "undefined"),
                LeafCtor::EnumMemberC(EnumMember {
                    enum_info,
                    member_name,
                }) => {
                    write!(f, "{}.{}", enum_info.enum_name, member_name)
                }
            }
        }
    }

    impl LeafCtor {
        pub fn to_ast(&self) -> (Loc, MatchPattern<Loc, Loc>) {
            use flow_parser::ast::BigIntLiteral as AstBigIntLiteral;
            use flow_parser::ast::BooleanLiteral;
            use flow_parser::ast::Identifier;
            use flow_parser::ast::IdentifierInner;
            use flow_parser::ast::NumberLiteral as AstNumberLiteral;
            use flow_parser::ast::StringLiteral;
            use flow_parser::ast::match_pattern;

            match self {
                LeafCtor::BoolC(value) => (
                    Loc::none(),
                    match_pattern::MatchPattern::BooleanPattern {
                        loc: Loc::none(),
                        inner: BooleanLiteral {
                            value: *value,
                            comments: None,
                        },
                    },
                ),
                LeafCtor::StrC(name) => {
                    let value = name.as_str();
                    let raw = js_layout_generator::quote_string(true, value);
                    (
                        Loc::none(),
                        match_pattern::MatchPattern::StringPattern {
                            loc: Loc::none(),
                            inner: StringLiteral {
                                value: value.into(),
                                raw: raw.into(),
                                comments: None,
                            },
                        },
                    )
                }
                LeafCtor::NumC(NumberLiteral(value, raw)) => (
                    Loc::none(),
                    match_pattern::MatchPattern::NumberPattern {
                        loc: Loc::none(),
                        inner: AstNumberLiteral {
                            value: *value,
                            raw: raw.to_string().into(),
                            comments: None,
                        },
                    },
                ),
                LeafCtor::BigIntC(BigIntLiteral(value, raw)) => (
                    Loc::none(),
                    match_pattern::MatchPattern::BigIntPattern {
                        loc: Loc::none(),
                        inner: AstBigIntLiteral {
                            value: *value,
                            raw: raw.to_string().into(),
                            comments: None,
                        },
                    },
                ),
                LeafCtor::NullC => (
                    Loc::none(),
                    match_pattern::MatchPattern::NullPattern {
                        loc: Loc::none(),
                        inner: None,
                    },
                ),
                LeafCtor::VoidC => (
                    Loc::none(),
                    match_pattern::MatchPattern::IdentifierPattern {
                        loc: Loc::none(),
                        inner: Identifier::new(IdentifierInner {
                            loc: Loc::none(),
                            name: "undefined".into(),
                            comments: None,
                        }),
                    },
                ),
                LeafCtor::EnumMemberC(EnumMember {
                    enum_info,
                    member_name,
                }) => {
                    use match_pattern::MemberPattern;
                    use match_pattern::member_pattern::Base;
                    use match_pattern::member_pattern::Property;

                    let base = Base::BaseIdentifier(Identifier::new(IdentifierInner {
                        loc: Loc::none(),
                        name: enum_info.enum_name.dupe(),
                        comments: None,
                    }));
                    let property = Property::PropertyIdentifier(Identifier::new(IdentifierInner {
                        loc: Loc::none(),
                        name: member_name.dupe(),
                        comments: None,
                    }));
                    (
                        Loc::none(),
                        match_pattern::MatchPattern::MemberPattern {
                            loc: Loc::none(),
                            inner: Arc::new(MemberPattern {
                                loc: Loc::none(),
                                base,
                                property,
                                comments: None,
                            }),
                        },
                    )
                }
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Leaf(pub Reason, pub LeafCtor);

    impl PartialEq for Leaf {
        fn eq(&self, other: &Self) -> bool {
            self.1 == other.1
        }
    }

    impl Eq for Leaf {}

    impl PartialOrd for Leaf {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Leaf {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.1.cmp(&other.1)
        }
    }

    impl Leaf {
        pub fn to_type(&self) -> Type {
            use flow_typing_type::type_::DefT;
            use flow_typing_type::type_::DefTInner;
            use flow_typing_type::type_::EnumInfo;
            use flow_typing_type::type_::EnumInfoInner;
            use flow_typing_type::type_::TypeInner;

            let Leaf(reason, leaf) = self;
            match leaf {
                LeafCtor::BoolC(value) => Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::SingletonBoolT {
                        value: *value,
                        from_annot: false,
                    }),
                )),
                LeafCtor::StrC(value) => Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::SingletonStrT {
                        value: value.dupe(),
                        from_annot: false,
                    }),
                )),
                LeafCtor::NumC(value) => Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::SingletonNumT {
                        value: value.dupe(),
                        from_annot: false,
                    }),
                )),
                LeafCtor::BigIntC(value) => Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::SingletonBigIntT {
                        value: value.dupe(),
                        from_annot: false,
                    }),
                )),
                LeafCtor::NullC => flow_typing_type::type_::null::make(reason.dupe()),
                LeafCtor::VoidC => flow_typing_type::type_::void::make(reason.dupe()),
                LeafCtor::EnumMemberC(EnumMember {
                    enum_info,
                    member_name: _,
                }) => Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::EnumValueT(std::rc::Rc::new(EnumInfo::new(
                        EnumInfoInner::ConcreteEnum((**enum_info).dupe()),
                    )))),
                )),
            }
        }
    }
}

pub type LeafSet = FlowOrdSet<leaf::Leaf>;

pub fn sort_object_patterns_by_index<T>(items: &mut [(usize, T)]) {
    items.sort_by(|(a, _), (b, _)| a.cmp(b));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjKind {
    Tuple { length: usize },
    Obj,
}

pub mod pattern_object {
    use flow_parser::loc_sig::LocSig;

    use super::*;

    #[derive(Debug, Clone)]
    pub struct Property {
        pub loc: ALoc,
        pub value: pattern_union::PatternUnion,
    }

    impl PartialEq for Property {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == std::cmp::Ordering::Equal
        }
    }

    impl Eq for Property {}

    impl PartialOrd for Property {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Property {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            let Property {
                value: value_a,
                loc: _,
            } = self;
            let Property {
                value: value_b,
                loc: _,
            } = other;
            // Sort leafs only props first. Sort wildcard props last.
            match (value_a.only_leafs(), value_b.only_leafs()) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => match (value_a.only_wildcard(), value_b.only_wildcard()) {
                    (None, Some(_)) => std::cmp::Ordering::Less,
                    (Some(_), None) => std::cmp::Ordering::Greater,
                    _ => std::cmp::Ordering::Equal,
                },
            }
        }
    }

    pub type Properties = FlowOrdMap<FlowSmolStr, Property>;

    #[derive(Debug, Clone)]
    pub struct PatternObjectInner {
        pub kind: ObjKind,
        pub props: Properties,
        pub class_info: Option<(ALocId, Option<FlowSmolStr>)>,
        pub keys_order: Vec<FlowSmolStr>,
        pub rest: Option<Reason>,
        pub contains_invalid_pattern: bool,
        pub guarded: bool,
    }

    #[derive(Debug, Clone, Dupe)]
    pub struct PatternObject(pub Reason, pub Rc<PatternObjectInner>);

    // index for ordering
    pub type WithIndex = (usize, PatternObject);

    impl PartialEq for PatternObject {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == std::cmp::Ordering::Equal
        }
    }

    impl Eq for PatternObject {}

    impl PartialOrd for PatternObject {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    // We aim to sort more specific patterns before less specific ones.
    impl Ord for PatternObject {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            fn compare_rest(rest1: &Option<Reason>, rest2: &Option<Reason>) -> std::cmp::Ordering {
                // without rest before with rest
                match (rest1, rest2) {
                    (None, Some(_)) => std::cmp::Ordering::Less,
                    (Some(_), None) => std::cmp::Ordering::Greater,
                    _ => std::cmp::Ordering::Equal,
                }
            }

            let PatternObject(_, inner1) = self;
            let PatternObjectInner {
                kind: kind1,
                props: props1,
                rest: rest1,
                ..
            } = inner1.as_ref();
            let PatternObject(_, inner2) = other;
            let PatternObjectInner {
                kind: kind2,
                props: props2,
                rest: rest2,
                ..
            } = inner2.as_ref();

            match (kind1, kind2) {
                // tuples before objects
                (ObjKind::Tuple { .. }, ObjKind::Obj) => std::cmp::Ordering::Less,
                (ObjKind::Obj, ObjKind::Tuple { .. }) => std::cmp::Ordering::Greater,
                (ObjKind::Tuple { length: length1 }, ObjKind::Tuple { length: length2 }) => {
                    // longer first
                    let length_compare = length2.cmp(length1);
                    if length_compare != std::cmp::Ordering::Equal {
                        length_compare
                    } else {
                        compare_rest(rest1, rest2)
                    }
                }
                (ObjKind::Obj, ObjKind::Obj) => {
                    // more props first
                    let props_size_compare = props2.len().cmp(&props1.len());
                    if props_size_compare != std::cmp::Ordering::Equal {
                        props_size_compare
                    } else {
                        compare_rest(rest1, rest2)
                    }
                }
            }
        }
    }

    impl std::fmt::Display for PatternObject {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let PatternObject(_, inner) = self;
            let PatternObjectInner {
                kind,
                props,
                class_info,
                keys_order,
                rest,
                ..
            } = inner.as_ref();

            let constructor = match class_info {
                Some((_, Some(class_name))) => format!("{} ", class_name),
                _ => String::new(),
            };

            match kind {
                ObjKind::Obj => {
                    let mut props_strs: Vec<String> = keys_order
                        .iter()
                        .map(|key| {
                            let Property { value, .. } = props.get(key).expect("key should exist");
                            format!("{}: {}", key, value)
                        })
                        .collect();

                    if rest.is_some() {
                        props_strs.push("...".to_string());
                    }

                    write!(f, "{}{{{}}}", constructor, props_strs.join(", "))
                }
                ObjKind::Tuple { length } => {
                    let mut elements: Vec<String> = (0..*length)
                        .map(|i| {
                            let key: FlowSmolStr = i.to_string().into();
                            let Property { value, .. } =
                                props.get(&key).expect("index should exist");
                            value.to_string()
                        })
                        .collect();

                    if rest.is_some() {
                        elements.push("...".to_string());
                    }

                    write!(f, "[{}]", elements.join(", "))
                }
            }
        }
    }

    impl PatternObject {
        pub fn to_ast(&self) -> (Loc, MatchPattern<Loc, Loc>) {
            use flow_parser::ast::Identifier;
            use flow_parser::ast::IdentifierInner;
            use flow_parser::ast::StringLiteral;
            use flow_parser::ast::match_pattern;
            use flow_parser::ast::match_pattern::ArrayPattern;
            use flow_parser::ast::match_pattern::InstancePattern;
            use flow_parser::ast::match_pattern::InstancePatternConstructor;
            use flow_parser::ast::match_pattern::ObjectPattern;
            use flow_parser::ast::match_pattern::RestPattern;
            use flow_parser::js_id_unicode::string_is_valid_identifier_name;

            let PatternObject(_, inner) = self;
            let PatternObjectInner {
                kind,
                props,
                class_info,
                keys_order,
                rest,
                ..
            } = inner.as_ref();

            let rest_pattern = if rest.is_some() {
                Some(RestPattern {
                    loc: Loc::none(),
                    argument: None,
                    comments: None,
                })
            } else {
                None
            };

            match kind {
                ObjKind::Obj => {
                    let properties: Vec<_> = keys_order
                        .iter()
                        .map(|key| {
                            let Property { value, .. } = props.get(key).expect("key should exist");
                            let key_pat = if string_is_valid_identifier_name(key) {
                                match_pattern::object_pattern::Key::Identifier(Identifier::new(
                                    IdentifierInner {
                                        loc: Loc::none(),
                                        name: key.dupe(),
                                        comments: None,
                                    },
                                ))
                            } else {
                                let raw = format!("'{}'", key);
                                match_pattern::object_pattern::Key::StringLiteral((
                                    Loc::none(),
                                    StringLiteral {
                                        value: key.dupe(),
                                        raw: raw.into(),
                                        comments: None,
                                    },
                                ))
                            };
                            let (_, pattern) = value.to_ast();
                            match_pattern::object_pattern::Property::Valid {
                                loc: Loc::none(),
                                property: match_pattern::object_pattern::PropertyStruct {
                                    key: key_pat,
                                    pattern,
                                    shorthand: false,
                                    comments: None,
                                },
                            }
                        })
                        .collect();

                    let obj_pattern = ObjectPattern {
                        properties: properties.into(),
                        rest: rest_pattern,
                        comments: None,
                    };

                    match class_info {
                        Some((_, Some(name))) => {
                            let constructor = InstancePatternConstructor::IdentifierConstructor(
                                Identifier::new(IdentifierInner {
                                    loc: Loc::none(),
                                    name: name.dupe(),
                                    comments: None,
                                }),
                            );
                            (
                                Loc::none(),
                                match_pattern::MatchPattern::InstancePattern {
                                    loc: Loc::none(),
                                    inner: Arc::new(InstancePattern {
                                        constructor,
                                        properties: (Loc::none(), obj_pattern),
                                        comments: None,
                                    }),
                                },
                            )
                        }
                        _ => (
                            Loc::none(),
                            match_pattern::MatchPattern::ObjectPattern {
                                loc: Loc::none(),
                                inner: Arc::new(obj_pattern),
                            },
                        ),
                    }
                }
                ObjKind::Tuple { length } => {
                    let elements: Vec<_> = (0..*length)
                        .map(|i| {
                            let key: FlowSmolStr = i.to_string().into();
                            let Property { value, .. } =
                                props.get(&key).expect("index should exist");
                            let (_, pattern) = value.to_ast();
                            match_pattern::array_pattern::Element {
                                index: Loc::none(),
                                pattern,
                            }
                        })
                        .collect();

                    (
                        Loc::none(),
                        match_pattern::MatchPattern::ArrayPattern {
                            loc: Loc::none(),
                            inner: Arc::new(ArrayPattern {
                                elements: elements.into(),
                                rest: rest_pattern,
                                comments: None,
                            }),
                        },
                    )
                }
            }
        }
    }
}

pub mod pattern_union {
    use dupe::IterDupedExt;
    use flow_parser::loc_sig::LocSig;

    use super::*;

    pub type TupleMap = FlowOrdMap<usize, FlowVector<pattern_object::WithIndex>>;

    #[derive(Debug, Clone)]
    pub struct PatternUnion {
        pub leafs: LeafSet,
        pub guarded_leafs: Vec<leaf::Leaf>,
        pub tuples_exact: TupleMap,
        pub tuples_inexact: TupleMap,
        pub objects: Vec<pattern_object::WithIndex>,
        pub wildcard: Option<Reason>,
        pub contains_invalid_pattern: bool,
    }

    pub fn empty() -> PatternUnion {
        PatternUnion {
            leafs: FlowOrdSet::default(),
            guarded_leafs: Vec::new(),
            tuples_exact: FlowOrdMap::default(),
            tuples_inexact: FlowOrdMap::default(),
            objects: Vec::new(),
            wildcard: None,
            contains_invalid_pattern: false,
        }
    }

    impl PatternUnion {
        pub fn only_wildcard(&self) -> Option<Reason> {
            let PatternUnion {
                leafs,
                guarded_leafs,
                tuples_exact,
                tuples_inexact,
                objects,
                wildcard,
                contains_invalid_pattern: _,
            } = self;
            if leafs.is_empty()
                && guarded_leafs.is_empty()
                && tuples_exact.is_empty()
                && tuples_inexact.is_empty()
                && objects.is_empty()
            {
                wildcard.dupe()
            } else {
                None
            }
        }

        pub fn only_leafs(&self) -> bool {
            let PatternUnion {
                leafs,
                guarded_leafs,
                tuples_exact,
                tuples_inexact,
                objects,
                wildcard,
                contains_invalid_pattern: _,
            } = self;
            !leafs.is_empty()
                && guarded_leafs.is_empty()
                && tuples_exact.is_empty()
                && tuples_inexact.is_empty()
                && objects.is_empty()
                && wildcard.is_none()
        }

        pub fn all_tuples_and_objects(&self) -> Vec<pattern_object::WithIndex> {
            let PatternUnion {
                tuples_exact,
                tuples_inexact,
                objects,
                ..
            } = self;

            let mut all_tuples: Vec<pattern_object::WithIndex> = tuples_exact
                .values()
                .chain(tuples_inexact.values())
                .flatten()
                .duped()
                .collect();

            all_tuples.extend(objects.iter().duped());
            sort_object_patterns_by_index(&mut all_tuples);
            all_tuples
        }

        pub fn to_ast(&self) -> (Loc, MatchPattern<Loc, Loc>) {
            use flow_parser::ast::match_pattern;
            use flow_parser::ast::match_pattern::OrPattern;
            use flow_parser::ast::match_pattern::WildcardPattern;

            let PatternUnion {
                leafs, wildcard, ..
            } = self;

            let leafs_asts: Vec<_> = leafs.iter().map(|leaf| leaf.1.to_ast()).collect();

            let tuples_and_objects: Vec<_> = self
                .all_tuples_and_objects()
                .iter()
                .map(|(_, po)| po.to_ast())
                .collect();

            let wildcard_asts: Vec<_> = if wildcard.is_some() {
                vec![(
                    Loc::none(),
                    match_pattern::MatchPattern::WildcardPattern {
                        loc: Loc::none(),
                        inner: WildcardPattern {
                            comments: None,
                            invalid_syntax_default_keyword: false,
                        },
                    },
                )]
            } else {
                vec![]
            };

            let patterns_with_loc: Vec<_> =
                [leafs_asts, tuples_and_objects, wildcard_asts].concat();

            match patterns_with_loc.as_slice() {
                [single] => single.clone(),
                _ => {
                    let patterns: Vec<_> = patterns_with_loc.into_iter().map(|(_, p)| p).collect();
                    (
                        Loc::none(),
                        match_pattern::MatchPattern::OrPattern {
                            loc: Loc::none(),
                            inner: Arc::new(OrPattern {
                                patterns: patterns.into(),
                                comments: None,
                            }),
                        },
                    )
                }
            }
        }
    }

    impl std::fmt::Display for PatternUnion {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let PatternUnion {
                leafs, wildcard, ..
            } = self;

            let leafs_strs: Vec<String> = leafs.iter().map(|leaf| leaf.1.to_string()).collect();

            let tuples_and_objects: Vec<String> = self
                .all_tuples_and_objects()
                .iter()
                .map(|(_, po)| po.to_string())
                .collect();

            let wildcard_strs: Vec<String> = if wildcard.is_some() {
                vec!["_".to_string()]
            } else {
                vec![]
            };

            write!(
                f,
                "{}",
                [leafs_strs, tuples_and_objects, wildcard_strs]
                    .concat()
                    .join(" | ")
            )
        }
    }
}

// `_`
pub fn wildcard_pattern(reason: Reason) -> pattern_union::PatternUnion {
    pattern_union::PatternUnion {
        wildcard: Some(reason),
        ..pattern_union::empty()
    }
}

// `[...]`
pub fn empty_inexact_tuple_pattern(reason: Reason) -> pattern_object::PatternObject {
    pattern_object::PatternObject(
        reason.dupe(),
        Rc::new(pattern_object::PatternObjectInner {
            kind: ObjKind::Tuple { length: 0 },
            props: FlowOrdMap::default(),
            class_info: None,
            keys_order: Vec::new(),
            rest: Some(reason),
            contains_invalid_pattern: false,
            guarded: false,
        }),
    )
}

pub mod value_object {
    use std::ops::Deref;

    use dupe::IterDupedExt;
    use once_cell::unsync::Lazy;

    use super::*;

    pub type LazyValueUnion =
        Rc<Lazy<value_union::ValueUnion, Box<dyn FnOnce() -> value_union::ValueUnion>>>;

    #[derive(Debug, Clone, Dupe)]
    pub struct Property {
        pub loc: ALoc,
        pub value: LazyValueUnion,
        pub optional: bool,
    }

    pub mod properties {
        use super::*;

        pub type Properties = FlowOrdMap<FlowSmolStr, Option<Property>>;

        pub fn is_empty(props: &Properties) -> bool {
            !props.values().any(|prop| prop.is_some())
        }
    }

    #[derive(Debug, Clone)]
    pub struct ValueObjectInner {
        pub kind: ObjKind,
        pub t: Type,
        pub props: properties::Properties,
        pub class_info: Option<(ALocId, Option<FlowSmolStr>, FlowOrdSet<ALocId>)>,
        pub rest: Option<Reason>,
        pub sentinel_props: FlowOrdSet<FlowSmolStr>,
    }

    #[derive(Debug, Clone, Dupe)]
    pub struct ValueObject(pub Reason, pub Rc<ValueObjectInner>);

    impl ValueObject {
        pub fn to_original_type(&self) -> Type {
            let ValueObject(_, inner) = self;
            inner.t.dupe()
        }

        pub fn to_pattern(&self) -> pattern_object::PatternObject {
            let ValueObject(reason, inner) = self;
            let ValueObjectInner {
                props,
                class_info,
                rest,
                kind,
                sentinel_props,
                ..
            } = inner.as_ref();

            let loc = reason.loc();

            let (props_result, keys_order, rest_result): (
                pattern_object::Properties,
                Vec<FlowSmolStr>,
                Option<Reason>,
            ) = match kind {
                ObjKind::Obj => {
                    let mut result_props: FlowOrdMap<FlowSmolStr, pattern_object::Property> =
                        FlowOrdMap::default();
                    let mut wildcard_props: FlowOrdMap<FlowSmolStr, pattern_object::Property> =
                        FlowOrdMap::default();
                    let mut result_rest = rest.dupe();

                    for (key, prop_opt) in props.iter() {
                        if let Some(prop) = prop_opt {
                            let Property {
                                value,
                                optional,
                                loc,
                            } = prop;
                            let is_sentinel_prop = sentinel_props.contains(key);

                            if *optional {
                                if result_rest.is_none() {
                                    result_rest = Some(reason.dupe());
                                }
                            } else if Lazy::get(&**value).is_some() || is_sentinel_prop {
                                let pattern_value = Lazy::force(&**value).to_pattern();
                                let pattern_prop = pattern_object::Property {
                                    loc: loc.dupe(),
                                    value: pattern_value,
                                };
                                if pattern_prop.value.only_wildcard().is_some() {
                                    wildcard_props.insert(key.dupe(), pattern_prop);
                                } else {
                                    result_props.insert(key.dupe(), pattern_prop);
                                }
                            } else {
                                let pattern_prop = pattern_object::Property {
                                    loc: loc.dupe(),
                                    value: wildcard_pattern(reason.dupe()),
                                };
                                wildcard_props.insert(key.dupe(), pattern_prop);
                            }
                        }
                    }

                    //  If we have over a certain amount of wildcard props, suggest an inexact
                    //  object pattern rather than many wildcard properties.
                    if wildcard_props.len() >= 5 {
                        if result_rest.is_none() {
                            result_rest = Some(reason.dupe());
                        }
                    } else {
                        result_props.extend(wildcard_props);
                    }

                    let mut keys: Vec<FlowSmolStr> = result_props.keys().duped().collect();
                    keys.sort_by(|key1, key2| {
                        // Sentinel props come first, then normal pattern prop order.
                        match (sentinel_props.contains(key1), sentinel_props.contains(key2)) {
                            (true, false) => std::cmp::Ordering::Less,
                            (false, true) => std::cmp::Ordering::Greater,
                            _ => {
                                let prop1 = result_props.get(key1).unwrap();
                                let prop2 = result_props.get(key2).unwrap();
                                prop1.cmp(prop2)
                            }
                        }
                    });

                    (result_props, keys, result_rest)
                }
                ObjKind::Tuple { length } => {
                    let result_props: FlowOrdMap<FlowSmolStr, pattern_object::Property> = props
                        .iter()
                        .map(|(key, prop_opt)| {
                            let (prop_loc, value) = match prop_opt {
                                Some(Property {
                                    value: prop_value,
                                    loc: prop_loc,
                                    ..
                                }) if Lazy::get(prop_value.deref()).is_some() => {
                                    (prop_loc.dupe(), Lazy::force(&**prop_value).to_pattern())
                                }
                                _ => (loc.dupe(), wildcard_pattern(reason.dupe())),
                            };
                            (
                                key.dupe(),
                                pattern_object::Property {
                                    loc: prop_loc,
                                    value,
                                },
                            )
                        })
                        .collect();

                    let keys_order: Vec<FlowSmolStr> =
                        (0..*length).map(|i| i.to_string().into()).collect();

                    (result_props, keys_order, rest.dupe())
                }
            };

            let class_info = class_info
                .as_ref()
                .map(|(class_id, name, _)| (class_id.dupe(), name.dupe()));

            pattern_object::PatternObject(
                reason.dupe(),
                Rc::new(pattern_object::PatternObjectInner {
                    kind: *kind,
                    props: props_result,
                    class_info,
                    keys_order,
                    rest: rest_result,
                    contains_invalid_pattern: false,
                    guarded: false,
                }),
            )
        }
    }
}

// A representation of a union of values.
pub mod value_union {
    use flow_typing_type::type_util;
    use once_cell::unsync::Lazy;

    use super::*;

    #[derive(Debug, Clone, Dupe)]
    pub struct ValueUnion {
        pub leafs: LeafSet,
        pub tuples: FlowVector<value_object::ValueObject>,
        pub arrays: FlowVector<value_object::ValueObject>,
        pub objects: FlowVector<value_object::ValueObject>,
        pub enum_unknown_members: FlowVector<(Reason, LeafSet)>,
        pub inexhaustible: FlowVector<Type>,
    }

    impl ValueUnion {
        pub fn empty() -> Self {
            Self {
                leafs: FlowOrdSet::default(),
                tuples: FlowVector::default(),
                arrays: FlowVector::default(),
                objects: FlowVector::default(),
                enum_unknown_members: FlowVector::default(),
                inexhaustible: FlowVector::default(),
            }
        }

        pub fn is_empty(&self) -> bool {
            let ValueUnion {
                leafs,
                tuples,
                arrays,
                objects,
                enum_unknown_members,
                inexhaustible,
            } = self;
            leafs.is_empty()
                && tuples.is_empty()
                && arrays.is_empty()
                && objects.is_empty()
                && enum_unknown_members.is_empty()
                && inexhaustible.is_empty()
        }

        // Returns true if the ValueUnion only contains inexhaustible types.
        // This is useful for optimization: filtering a purely inexhaustible ValueUnion
        // by patterns won't change its effective type (the inexhaustible part is preserved).
        pub fn is_only_inexhaustible(&self) -> bool {
            let ValueUnion {
                leafs,
                tuples,
                arrays,
                objects,
                enum_unknown_members,
                inexhaustible,
            } = self;
            leafs.is_empty()
                && tuples.is_empty()
                && arrays.is_empty()
                && objects.is_empty()
                && enum_unknown_members.is_empty()
                && !inexhaustible.is_empty()
        }

        fn union(vu1: Self, vu2: Self) -> Self {
            let Self {
                mut leafs,
                mut tuples,
                mut arrays,
                mut objects,
                mut enum_unknown_members,
                mut inexhaustible,
            } = vu1;
            let Self {
                leafs: leafs2,
                tuples: tuples2,
                arrays: arrays2,
                objects: objects2,
                enum_unknown_members: enum_unknown_members2,
                inexhaustible: inexhaustible2,
            } = vu2;

            leafs.extend(leafs2);
            tuples.append(tuples2.into());
            arrays.append(arrays2.into());
            objects.append(objects2.into());
            enum_unknown_members.append(enum_unknown_members2.into());
            inexhaustible.append(inexhaustible2.into());
            Self {
                leafs,
                tuples,
                arrays,
                objects,
                enum_unknown_members,
                inexhaustible,
            }
        }

        pub fn to_pattern(&self) -> pattern_union::PatternUnion {
            let ValueUnion {
                leafs,
                tuples,
                arrays,
                objects,
                enum_unknown_members,
                inexhaustible,
            } = self;

            //   let (tuples_exact, tuples_inexact) =
            //     tuples
            //     |> Base.List.map ~f:ValueObject.to_pattern
            //     |> Base.List.stable_sort ~compare:PatternObject.compare
            //     |> Base.List.foldi ~init:(IMap.empty, IMap.empty) ~f:(fun i acc tuple_pattern ->
            //            ...
            //        )
            //   in
            let mut tuple_patterns: Vec<pattern_object::PatternObject> =
                tuples.iter().map(|vo| vo.to_pattern()).collect();
            tuple_patterns.sort();

            let mut tuples_exact: FlowOrdMap<usize, FlowVector<pattern_object::WithIndex>> =
                FlowOrdMap::default();
            let mut tuples_inexact: FlowOrdMap<usize, FlowVector<pattern_object::WithIndex>> =
                FlowOrdMap::default();

            for (i, tuple_pattern) in tuple_patterns.into_iter().enumerate() {
                let pattern_object::PatternObject(_, inner) = &tuple_pattern;
                let pattern_object::PatternObjectInner { kind, rest, .. } = inner.as_ref();

                match kind {
                    ObjKind::Tuple { length } => {
                        if rest.is_some() {
                            tuples_inexact
                                .entry(*length)
                                .or_default()
                                .push((i, tuple_pattern.dupe()));
                        } else {
                            tuples_exact
                                .entry(*length)
                                .or_default()
                                .push((i, tuple_pattern.dupe()));
                        }
                    }
                    ObjKind::Obj => {
                        // Tuples are always `ObjKind.Tuple`
                    }
                }
            }

            // If we have arrays, add the empty inexact tuple pattern.
            if let Some(value_object::ValueObject(reason, _)) = arrays.front() {
                let i = tuples.len();
                tuples_inexact
                    .entry(0)
                    .or_default()
                    .push((i, empty_inexact_tuple_pattern(reason.dupe())));
            }

            let mut object_patterns: Vec<pattern_object::PatternObject> =
                objects.iter().map(|vo| vo.to_pattern()).collect();
            object_patterns.sort();
            let objects_result: Vec<pattern_object::WithIndex> =
                object_patterns.into_iter().enumerate().collect();

            let wildcard = match (inexhaustible.front(), enum_unknown_members.front()) {
                (None, None) => None,
                (Some(first_t), _) => Some(type_util::reason_of_t(first_t).dupe()),
                (None, Some((reason, _))) => Some(reason.dupe()),
            };

            pattern_union::PatternUnion {
                leafs: leafs.dupe(),
                tuples_exact,
                tuples_inexact,
                objects: objects_result,
                wildcard,
                ..pattern_union::empty()
            }
        }

        pub fn to_type(&self, r: Reason) -> Type {
            let Self {
                leafs,
                tuples,
                arrays,
                objects,
                enum_unknown_members,
                inexhaustible,
            } = self;

            let all_possible_types: Vec<Type> = [
                leafs.iter().map(|l| l.to_type()).collect::<Vec<_>>(),
                tuples
                    .iter()
                    .map(|vo| vo.to_original_type())
                    .collect::<Vec<_>>(),
                arrays
                    .iter()
                    .map(|vo| vo.to_original_type())
                    .collect::<Vec<_>>(),
                objects
                    .iter()
                    .map(|vo| vo.to_original_type())
                    .collect::<Vec<_>>(),
                enum_unknown_members
                    .iter()
                    .flat_map(|(_, leaf_set)| leaf_set.iter().map(|l| l.to_type()))
                    .collect::<Vec<_>>(),
                inexhaustible.iter().duped().collect::<Vec<_>>(),
            ]
            .concat();

            type_util::union_of_ts(r, all_possible_types, None)
        }

        pub fn select(&self, selector: &Selector<ALoc, ALoc>) -> Option<Self> {
            let Self {
                tuples,
                arrays,
                objects,
                ..
            } = self;

            fn conservative_find(
                key: &FlowSmolStr,
                values: &FlowVector<value_object::ValueObject>,
            ) -> Option<Vec<value_object::LazyValueUnion>> {
                let mut acc: Vec<value_object::LazyValueUnion> = Vec::new();
                for value_object::ValueObject(_, inner) in values.iter() {
                    let value_object::ValueObjectInner { props, rest, .. } = inner.as_ref();
                    match props.get(key) {
                        None => {
                            if rest.is_some() {
                                return None;
                            }
                        }
                        Some(None) => {
                            // We definitely know the field doesn't exist.
                        }
                        Some(Some(v)) => {
                            acc.push(v.value.dupe());
                        }
                    }
                }
                Some(acc)
            }

            let conservative_find_all =
                |key: &FlowSmolStr| -> Option<Vec<value_object::LazyValueUnion>> {
                    let tuple_candidates = conservative_find(key, tuples)?;
                    let array_candidates = conservative_find(key, arrays)?;
                    let object_candidates = conservative_find(key, objects)?;
                    Some([tuple_candidates, array_candidates, object_candidates].concat())
                };

            let vus_opt: Option<Vec<ValueUnion>> = match selector {
                Selector::Elem { index, .. } => {
                    let key: FlowSmolStr = index.to_string().into();
                    conservative_find_all(&key).map(|lazy_cells| {
                        lazy_cells
                            .into_iter()
                            .map(|cell| Lazy::force(&*cell).dupe())
                            .collect()
                    })
                }
                Selector::Prop { prop, .. } => conservative_find_all(prop).map(|lazy_cells| {
                    lazy_cells
                        .into_iter()
                        .map(|cell| Lazy::force(&*cell).clone())
                        .collect()
                }),
                Selector::Computed { .. } => None,
                Selector::ObjRest { .. } => None,
                Selector::ArrRest(_) => None,
                Selector::Default => Some(vec![self.clone()]),
            };

            vus_opt.map(|vus| {
                vus.into_iter()
                    .rfold(Self::empty(), |acc, vu| Self::union(vu, acc))
            })
        }
    }
}
