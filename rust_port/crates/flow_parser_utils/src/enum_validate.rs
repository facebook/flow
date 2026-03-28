// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use std::collections::BTreeMap;
use std::collections::HashMap;

use dupe::Dupe;
use flow_parser::ast::BigIntLiteral;
use flow_parser::ast::BooleanLiteral;
use flow_parser::ast::NumberLiteral;
use flow_parser::ast::StringLiteral;
use flow_parser::ast::statement::enum_declaration::Body;
use flow_parser::ast::statement::enum_declaration::DefaultedMember;
use flow_parser::ast::statement::enum_declaration::ExplicitType;
use flow_parser::ast::statement::enum_declaration::InitializedMember;
use flow_parser::ast::statement::enum_declaration::Member;

pub enum ValidationError<M: Dupe> {
    DuplicateMemberName {
        loc: M,
        prev_use_loc: M,
        member_name: String,
    },
    InconsistentMemberValues {
        loc: M,
    },
    InvalidMemberInitializer {
        loc: M,
        explicit_type: Option<ExplicitType>,
        member_name: String,
    },
    BooleanMemberNotInitialized {
        loc: M,
        member_name: String,
    },
    NumberMemberNotInitialized {
        loc: M,
        member_name: String,
    },
    BigIntMemberNotInitialized {
        loc: M,
        member_name: String,
    },
    StringMemberInconsistentlyInitialized {
        loc: M,
    },
    SymbolMemberWithInitializer {
        loc: M,
        member_name: String,
    },
    DuplicateMemberValue {
        loc: M,
        prev_use_loc: M,
    },
    InvalidMemberName {
        loc: M,
        member_name: String,
    },
}

struct ClassifiedMembers {
    boolean_count: usize,
    number_count: usize,
    string_count: usize,
    bigint_count: usize,
    defaulted_count: usize,
}

pub enum EnumRep {
    BoolRep(Option<bool>),
    NumberRep { truthy: bool },
    StringRep { truthy: bool },
    SymbolRep,
    BigIntRep { truthy: bool },
}

pub struct ClassificationResult<M: Dupe> {
    pub rep: Option<EnumRep>,
    pub members: Vec<(String, M)>,
    pub has_unknown_members: Option<M>,
    pub errors: Vec<ValidationError<M>>,
}

fn member_name<M: Dupe>(member: &Member<M>) -> &str {
    match member {
        Member::BooleanMember(InitializedMember { id, .. })
        | Member::NumberMember(InitializedMember { id, .. })
        | Member::StringMember(InitializedMember { id, .. })
        | Member::BigIntMember(InitializedMember { id, .. })
        | Member::DefaultedMember(DefaultedMember { id, .. }) => &id.name,
    }
}

fn member_id_loc<M: Dupe>(member: &Member<M>) -> &M {
    match member {
        Member::BooleanMember(InitializedMember { id, .. })
        | Member::NumberMember(InitializedMember { id, .. })
        | Member::StringMember(InitializedMember { id, .. })
        | Member::BigIntMember(InitializedMember { id, .. })
        | Member::DefaultedMember(DefaultedMember { id, .. }) => &id.loc,
    }
}

fn member_loc<M: Dupe>(member: &Member<M>) -> &M {
    match member {
        Member::BooleanMember(InitializedMember { loc, .. })
        | Member::NumberMember(InitializedMember { loc, .. })
        | Member::StringMember(InitializedMember { loc, .. })
        | Member::BigIntMember(InitializedMember { loc, .. })
        | Member::DefaultedMember(DefaultedMember { loc, .. }) => loc,
    }
}

fn count_members<M: Dupe>(members: &[Member<M>]) -> ClassifiedMembers {
    let mut counts = ClassifiedMembers {
        boolean_count: 0,
        number_count: 0,
        string_count: 0,
        bigint_count: 0,
        defaulted_count: 0,
    };
    for member in members {
        match member {
            Member::BooleanMember(_) => counts.boolean_count += 1,
            Member::NumberMember(_) => counts.number_count += 1,
            Member::StringMember(_) => counts.string_count += 1,
            Member::BigIntMember(_) => counts.bigint_count += 1,
            Member::DefaultedMember(_) => counts.defaulted_count += 1,
        }
    }
    counts
}

fn check_duplicate_names<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut seen: HashMap<&str, M> = HashMap::new();
    let mut errs: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        let name = member_name(member);
        if name.is_empty() {
            continue;
        }
        match seen.get(name) {
            Some(prev_use_loc) => {
                let loc = member_id_loc(member).dupe();
                errs.push(ValidationError::DuplicateMemberName {
                    loc,
                    prev_use_loc: prev_use_loc.dupe(),
                    member_name: name.to_string(),
                });
            }
            None => {
                let loc = member_id_loc(member).dupe();
                seen.insert(name, loc);
            }
        }
    }
    errs
}

fn check_member_names<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let is_a_to_z = |c: char| c >= 'a' && c <= 'z';
    let mut errs: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        let name = member_name(member);
        if !name.is_empty() && is_a_to_z(name.as_bytes()[0] as char) {
            let loc = member_id_loc(member).dupe();
            errs.push(ValidationError::InvalidMemberName {
                loc,
                member_name: name.to_string(),
            });
        }
    }
    errs
}

fn check_duplicate_values<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut bool_seen: BTreeMap<bool, M> = BTreeMap::new();
    let mut num_seen: BTreeMap<u64, M> = BTreeMap::new();
    let mut str_seen: HashMap<&str, M> = HashMap::new();
    let mut bigint_seen: BTreeMap<Option<i64>, M> = BTreeMap::new();
    let mut errs: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        match member {
            Member::BooleanMember(InitializedMember {
                init: (init_loc, BooleanLiteral { value, .. }),
                ..
            }) => match bool_seen.get(value) {
                Some(prev_use_loc) => {
                    errs.push(ValidationError::DuplicateMemberValue {
                        loc: init_loc.dupe(),
                        prev_use_loc: prev_use_loc.dupe(),
                    });
                }
                None => {
                    let loc = member_loc(member).dupe();
                    bool_seen.insert(*value, loc);
                }
            },
            Member::NumberMember(InitializedMember {
                init: (init_loc, NumberLiteral { value, .. }),
                ..
            }) => {
                let bits = value.to_bits();
                match num_seen.get(&bits) {
                    Some(prev_use_loc) => {
                        errs.push(ValidationError::DuplicateMemberValue {
                            loc: init_loc.dupe(),
                            prev_use_loc: prev_use_loc.dupe(),
                        });
                    }
                    None => {
                        let loc = member_loc(member).dupe();
                        num_seen.insert(bits, loc);
                    }
                }
            }
            Member::StringMember(InitializedMember {
                init: (init_loc, StringLiteral { value, .. }),
                ..
            }) => match str_seen.get(value.as_str()) {
                Some(prev_use_loc) => {
                    errs.push(ValidationError::DuplicateMemberValue {
                        loc: init_loc.dupe(),
                        prev_use_loc: prev_use_loc.dupe(),
                    });
                }
                None => {
                    let loc = member_loc(member).dupe();
                    str_seen.insert(value, loc);
                }
            },
            Member::BigIntMember(InitializedMember {
                init: (init_loc, BigIntLiteral { value, .. }),
                ..
            }) => match bigint_seen.get(value) {
                Some(prev_use_loc) => {
                    errs.push(ValidationError::DuplicateMemberValue {
                        loc: init_loc.dupe(),
                        prev_use_loc: prev_use_loc.dupe(),
                    });
                }
                None => {
                    let loc = member_loc(member).dupe();
                    bigint_seen.insert(*value, loc);
                }
            },
            Member::DefaultedMember(_) => {}
        }
    }
    errs
}

fn validate_explicit_boolean<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut errors: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        match member {
            Member::BooleanMember(_) => {}
            Member::DefaultedMember(DefaultedMember { loc, id }) => {
                errors.push(ValidationError::BooleanMemberNotInitialized {
                    loc: loc.dupe(),
                    member_name: id.name.to_string(),
                });
            }
            _ => {
                let loc = member_loc(member).dupe();
                let name = member_name(member);
                errors.push(ValidationError::InvalidMemberInitializer {
                    loc,
                    explicit_type: Some(ExplicitType::Boolean),
                    member_name: name.to_string(),
                });
            }
        }
    }
    errors
}

fn validate_explicit_number<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut errors: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        match member {
            Member::NumberMember(_) => {}
            Member::DefaultedMember(DefaultedMember { loc, id }) => {
                errors.push(ValidationError::NumberMemberNotInitialized {
                    loc: loc.dupe(),
                    member_name: id.name.to_string(),
                });
            }
            _ => {
                let loc = member_loc(member).dupe();
                let name = member_name(member);
                errors.push(ValidationError::InvalidMemberInitializer {
                    loc,
                    explicit_type: Some(ExplicitType::Number),
                    member_name: name.to_string(),
                });
            }
        }
    }
    errors
}

fn validate_explicit_string<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut errors: Vec<ValidationError<M>> = Vec::new();
    let string_count = members
        .iter()
        .filter(|member| matches!(member, Member::StringMember(_)))
        .count();
    let defaulted_count = members
        .iter()
        .filter(|member| matches!(member, Member::DefaultedMember(_)))
        .count();
    for member in members {
        match member {
            Member::StringMember(_) | Member::DefaultedMember(_) => {}
            _ => {
                let loc = member_loc(member).dupe();
                let name = member_name(member);
                errors.push(ValidationError::InvalidMemberInitializer {
                    loc,
                    explicit_type: Some(ExplicitType::String),
                    member_name: name.to_string(),
                });
            }
        }
    }
    if string_count > 0 && defaulted_count > 0 {
        if defaulted_count > string_count {
            for member in members {
                match member {
                    Member::StringMember(InitializedMember { loc, .. }) => {
                        errors.push(ValidationError::StringMemberInconsistentlyInitialized {
                            loc: loc.dupe(),
                        });
                    }
                    _ => {}
                }
            }
        } else {
            for member in members {
                match member {
                    Member::DefaultedMember(DefaultedMember { loc, .. }) => {
                        errors.push(ValidationError::StringMemberInconsistentlyInitialized {
                            loc: loc.dupe(),
                        });
                    }
                    _ => {}
                }
            }
        }
    }
    errors
}

fn validate_explicit_symbol<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut errors: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        match member {
            Member::DefaultedMember(_) => {}
            _ => {
                let loc = member_loc(member).dupe();
                let name = member_name(member);
                errors.push(ValidationError::SymbolMemberWithInitializer {
                    loc,
                    member_name: name.to_string(),
                });
            }
        }
    }
    errors
}

fn validate_explicit_bigint<M: Dupe>(members: &[Member<M>]) -> Vec<ValidationError<M>> {
    let mut errors: Vec<ValidationError<M>> = Vec::new();
    for member in members {
        match member {
            Member::BigIntMember(_) => {}
            Member::DefaultedMember(DefaultedMember { loc, id }) => {
                errors.push(ValidationError::BigIntMemberNotInitialized {
                    loc: loc.dupe(),
                    member_name: id.name.to_string(),
                });
            }
            _ => {
                let loc = member_loc(member).dupe();
                let name = member_name(member);
                errors.push(ValidationError::InvalidMemberInitializer {
                    loc,
                    explicit_type: Some(ExplicitType::BigInt),
                    member_name: name.to_string(),
                });
            }
        }
    }
    errors
}

fn validate_implicit_type<M: Dupe>(
    body_loc: &M,
    members: &[Member<M>],
) -> (Option<EnumRep>, Vec<ValidationError<M>>) {
    let counts = count_members(members);
    let ClassifiedMembers {
        boolean_count,
        number_count,
        string_count,
        bigint_count,
        defaulted_count,
    } = counts;
    match (
        boolean_count,
        number_count,
        bigint_count,
        string_count,
        defaulted_count,
    ) {
        (0, 0, 0, 0, 0) => (Some(EnumRep::StringRep { truthy: true }), vec![]),
        (0, 0, 0, _, _) => {
            let mut errors: Vec<ValidationError<M>> = Vec::new();
            if string_count > 0 && defaulted_count > 0 {
                if defaulted_count > string_count {
                    for member in members {
                        match member {
                            Member::StringMember(InitializedMember { loc, .. }) => {
                                errors.push(
                                    ValidationError::StringMemberInconsistentlyInitialized {
                                        loc: loc.dupe(),
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    (Some(EnumRep::StringRep { truthy: true }), errors)
                } else {
                    for member in members {
                        match member {
                            Member::DefaultedMember(DefaultedMember { loc, .. }) => {
                                errors.push(
                                    ValidationError::StringMemberInconsistentlyInitialized {
                                        loc: loc.dupe(),
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    let truthy = members.iter().all(|member| match member {
                        Member::StringMember(InitializedMember {
                            init: (_, StringLiteral { value, .. }),
                            ..
                        }) => !value.is_empty(),
                        _ => true,
                    });
                    (Some(EnumRep::StringRep { truthy }), errors)
                }
            } else if string_count > 0 {
                let truthy = members.iter().all(|member| match member {
                    Member::StringMember(InitializedMember {
                        init: (_, StringLiteral { value, .. }),
                        ..
                    }) => !value.is_empty(),
                    _ => true,
                });
                (Some(EnumRep::StringRep { truthy }), vec![])
            } else {
                (Some(EnumRep::StringRep { truthy: true }), vec![])
            }
        }
        (_, 0, 0, 0, _) if boolean_count >= defaulted_count => {
            let mut errors: Vec<ValidationError<M>> = Vec::new();
            for member in members {
                match member {
                    Member::DefaultedMember(DefaultedMember { loc, id }) => {
                        errors.push(ValidationError::BooleanMemberNotInitialized {
                            loc: loc.dupe(),
                            member_name: id.name.to_string(),
                        });
                    }
                    _ => {}
                }
            }
            let lit = members.iter().fold(None, |acc, member| match member {
                Member::BooleanMember(InitializedMember {
                    init: (_, BooleanLiteral { value, .. }),
                    ..
                }) => match acc {
                    None => Some(Some(*value)),
                    Some(_) => Some(None),
                },
                _ => acc,
            });
            let lit = lit.flatten();
            (Some(EnumRep::BoolRep(lit)), errors)
        }
        (0, _, 0, 0, _) if number_count >= defaulted_count => {
            let mut errors: Vec<ValidationError<M>> = Vec::new();
            for member in members {
                match member {
                    Member::DefaultedMember(DefaultedMember { loc, id }) => {
                        errors.push(ValidationError::NumberMemberNotInitialized {
                            loc: loc.dupe(),
                            member_name: id.name.to_string(),
                        });
                    }
                    _ => {}
                }
            }
            let truthy = members.iter().all(|member| match member {
                Member::NumberMember(InitializedMember {
                    init: (_, NumberLiteral { value, .. }),
                    ..
                }) => *value != 0.0,
                _ => true,
            });
            (Some(EnumRep::NumberRep { truthy }), errors)
        }
        (0, 0, _, 0, _) if bigint_count >= defaulted_count => {
            let mut errors: Vec<ValidationError<M>> = Vec::new();
            for member in members {
                match member {
                    Member::DefaultedMember(DefaultedMember { loc, id }) => {
                        errors.push(ValidationError::BigIntMemberNotInitialized {
                            loc: loc.dupe(),
                            member_name: id.name.to_string(),
                        });
                    }
                    _ => {}
                }
            }
            let truthy = members.iter().all(|member| match member {
                Member::BigIntMember(InitializedMember {
                    init: (_, BigIntLiteral { value, .. }),
                    ..
                }) => *value != Some(0i64),
                _ => true,
            });
            (Some(EnumRep::BigIntRep { truthy }), errors)
        }
        _ => (
            None,
            vec![ValidationError::InconsistentMemberValues {
                loc: body_loc.dupe(),
            }],
        ),
    }
}

fn compute_rep_for_explicit_type<M: Dupe>(
    explicit_type: ExplicitType,
    members: &[Member<M>],
) -> EnumRep {
    match explicit_type {
        ExplicitType::Boolean => {
            let lit = members.iter().fold(None, |acc, member| match member {
                Member::BooleanMember(InitializedMember {
                    init: (_, BooleanLiteral { value, .. }),
                    ..
                }) => match acc {
                    None => Some(Some(*value)),
                    Some(_) => Some(None),
                },
                _ => acc,
            });
            EnumRep::BoolRep(lit.flatten())
        }
        ExplicitType::Number => {
            let truthy = members.iter().all(|member| match member {
                Member::NumberMember(InitializedMember {
                    init: (_, NumberLiteral { value, .. }),
                    ..
                }) => *value != 0.0,
                _ => true,
            });
            EnumRep::NumberRep { truthy }
        }
        ExplicitType::String => {
            let has_initialized = members
                .iter()
                .any(|member| matches!(member, Member::StringMember(_)));
            if has_initialized {
                let truthy = members.iter().all(|member| match member {
                    Member::StringMember(InitializedMember {
                        init: (_, StringLiteral { value, .. }),
                        ..
                    }) => !value.is_empty(),
                    _ => true,
                });
                EnumRep::StringRep { truthy }
            } else {
                EnumRep::StringRep { truthy: true }
            }
        }
        ExplicitType::Symbol => EnumRep::SymbolRep,
        ExplicitType::BigInt => {
            let truthy = members.iter().all(|member| match member {
                Member::BigIntMember(InitializedMember {
                    init: (_, BigIntLiteral { value, .. }),
                    ..
                }) => *value != Some(0i64),
                _ => true,
            });
            EnumRep::BigIntRep { truthy }
        }
    }
}

fn extract_member_names<M: Dupe>(members: &[Member<M>]) -> Vec<(String, M)> {
    let mut acc = Vec::new();
    for member in members {
        let name = member_name(member);
        let loc = member_loc(member);
        if name.is_empty() {
            continue;
        }
        acc.push((name.to_string(), loc.dupe()));
    }
    acc
}

pub fn classify_enum_body<M: Dupe>(body: &Body<M>, body_loc: &M) -> ClassificationResult<M> {
    let Body {
        members,
        explicit_type,
        has_unknown_members,
        comments: _,
        ..
    } = body;
    let dup_errors = check_duplicate_names(members);
    let member_names = extract_member_names(members);
    let (rep, type_errors) = match explicit_type {
        Some((_, ExplicitType::Boolean)) => {
            let errs = validate_explicit_boolean(members);
            (
                Some(compute_rep_for_explicit_type(
                    ExplicitType::Boolean,
                    members,
                )),
                errs,
            )
        }
        Some((_, ExplicitType::Number)) => {
            let errs = validate_explicit_number(members);
            (
                Some(compute_rep_for_explicit_type(ExplicitType::Number, members)),
                errs,
            )
        }
        Some((_, ExplicitType::String)) => {
            let errs = validate_explicit_string(members);
            (
                Some(compute_rep_for_explicit_type(ExplicitType::String, members)),
                errs,
            )
        }
        Some((_, ExplicitType::Symbol)) => {
            let errs = validate_explicit_symbol(members);
            (Some(EnumRep::SymbolRep), errs)
        }
        Some((_, ExplicitType::BigInt)) => {
            let errs = validate_explicit_bigint(members);
            (
                Some(compute_rep_for_explicit_type(ExplicitType::BigInt, members)),
                errs,
            )
        }
        None => validate_implicit_type(body_loc, members),
    };
    let name_errors = check_member_names(members);
    let value_errors = check_duplicate_values(members);
    let mut errors = dup_errors;
    errors.extend(type_errors);
    errors.extend(name_errors);
    errors.extend(value_errors);
    ClassificationResult {
        rep,
        members: member_names,
        has_unknown_members: has_unknown_members.dupe(),
        errors,
    }
}
