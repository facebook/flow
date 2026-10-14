// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use dupe::Dupe;
use flow_parser::ast::function;
use flow_parser::ast::pattern;
use flow_parser::loc::Loc;

use super::Pattern;
use super::PrintError;
use super::Printed;
use super::Writer;

impl Writer {
    pub(super) fn pattern(&mut self, pattern: &Pattern) -> Printed {
        self.at(pattern.loc(), |writer| match pattern {
            Pattern::Identifier { inner, .. } => {
                writer.annotation(&inner.annot)?;
                if inner.optional {
                    return Err(PrintError::TypeAnnotation(pattern.loc().dupe()));
                }
                writer.identifier(&inner.name)
            }
            Pattern::Expression { inner, .. } => writer.expression(inner),
            Pattern::Object { inner, .. } => {
                writer.annotation(&inner.annot)?;
                if inner.optional {
                    return Err(PrintError::TypeAnnotation(pattern.loc().dupe()));
                }
                writer.leading(&inner.comments)?;
                writer.token("{")?;
                for (index, property) in inner.properties.iter().enumerate() {
                    if index != 0 {
                        writer.token(", ")?;
                    }
                    match property {
                        pattern::object::Property::NormalProperty(property) => {
                            writer.at(&property.loc, |writer| {
                                if !property.shorthand {
                                    writer.pattern_key(&property.key)?;
                                    writer.token(": ")?;
                                }
                                writer.pattern(&property.pattern)?;
                                if let Some(default) = &property.default {
                                    writer.token(" = ")?;
                                    writer.expression(default)?;
                                }
                                Ok(())
                            })?
                        }
                        pattern::object::Property::RestElement(rest) => {
                            writer.pattern_rest(rest)?
                        }
                    }
                }
                writer.internal(&inner.comments)?;
                writer.token("}")?;
                writer.trailing(&inner.comments)
            }
            Pattern::Array { inner, .. } => {
                writer.annotation(&inner.annot)?;
                if inner.optional {
                    return Err(PrintError::TypeAnnotation(pattern.loc().dupe()));
                }
                writer.leading(&inner.comments)?;
                writer.token("[")?;
                for (index, element) in inner.elements.iter().enumerate() {
                    if index != 0 {
                        writer.token(", ")?;
                    }
                    match element {
                        pattern::array::Element::NormalElement(element) => {
                            writer.at(&element.loc, |writer| {
                                writer.pattern(&element.argument)?;
                                if let Some(default) = &element.default {
                                    writer.token(" = ")?;
                                    writer.expression(default)?;
                                }
                                Ok(())
                            })?
                        }
                        pattern::array::Element::RestElement(rest) => writer.pattern_rest(rest)?,
                        pattern::array::Element::Hole(_) => {}
                    }
                }
                if matches!(
                    inner.elements.last(),
                    Some(pattern::array::Element::Hole(_))
                ) {
                    writer.token(",")?;
                }
                writer.internal(&inner.comments)?;
                writer.token("]")?;
                writer.trailing(&inner.comments)
            }
        })
    }

    fn pattern_rest(&mut self, rest: &pattern::RestElement<Loc, Loc>) -> Printed {
        self.at(&rest.loc, |writer| {
            writer.leading(&rest.comments)?;
            writer.token("...")?;
            writer.pattern(&rest.argument)?;
            writer.trailing(&rest.comments)
        })
    }

    fn pattern_key(&mut self, key: &pattern::object::Key<Loc, Loc>) -> Printed {
        use pattern::object::Key;
        match key {
            Key::Identifier(id) => self.identifier(id),
            Key::StringLiteral((loc, literal)) => self.at(loc, |writer| writer.string(literal)),
            Key::NumberLiteral((loc, literal)) => self.at(loc, |writer| {
                writer.leading(&literal.comments)?;
                writer.token(&literal.raw)?;
                writer.trailing(&literal.comments)
            }),
            Key::BigIntLiteral((loc, literal)) => self.at(loc, |writer| {
                writer.leading(&literal.comments)?;
                writer.token(&literal.raw)?;
                writer.trailing(&literal.comments)
            }),
            Key::Computed(key) => self.computed_key(key),
        }
    }

    pub(super) fn params(&mut self, params: &function::Params<Loc, Loc>) -> Printed {
        if let Some(this) = &params.this_ {
            return Err(PrintError::TypeAnnotation(this.loc.dupe()));
        }
        self.at(&params.loc, |writer| {
            writer.token("(")?;
            writer.leading(&params.comments)?;
            for (index, param) in params.params.iter().enumerate() {
                if index != 0 {
                    writer.token(", ")?;
                }
                match param {
                    function::Param::RegularParam {
                        loc,
                        argument,
                        default,
                    } => writer.at(loc, |writer| {
                        writer.pattern(argument)?;
                        if let Some(default) = default {
                            writer.token(" = ")?;
                            writer.expression(default)?;
                        }
                        Ok(())
                    })?,
                    function::Param::ParamProperty { loc, .. } => {
                        return Err(PrintError::TypeAnnotation(loc.dupe()));
                    }
                }
            }
            if let Some(rest) = &params.rest {
                if !params.params.is_empty() {
                    writer.token(", ")?;
                }
                writer.at(&rest.loc, |writer| {
                    writer.leading(&rest.comments)?;
                    writer.token("...")?;
                    writer.pattern(&rest.argument)?;
                    writer.trailing(&rest.comments)
                })?;
            }
            writer.internal(&params.comments)?;
            writer.trailing(&params.comments)?;
            writer.token(")")?;
            Ok(())
        })
    }
}
