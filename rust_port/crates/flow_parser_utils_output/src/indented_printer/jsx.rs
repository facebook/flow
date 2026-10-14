// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use dupe::Dupe;
use flow_parser::ast::jsx;
use flow_parser::loc::Loc;

use super::PrintError;
use super::Printed;
use super::Writer;

impl Writer {
    fn jsx_identifier(&mut self, id: &jsx::Identifier<Loc, Loc>) -> Printed {
        self.at(&id.loc, |writer| {
            writer.leading(&id.comments)?;
            writer.write(&id.name, Some(&id.name))?;
            writer.trailing(&id.comments)
        })
    }

    fn jsx_namespace(&mut self, name: &jsx::NamespacedName<Loc, Loc>) -> Printed {
        self.at(&name.loc, |writer| {
            writer.jsx_identifier(&name.namespace)?;
            writer.token(":")?;
            writer.jsx_identifier(&name.name)
        })
    }

    fn jsx_member(&mut self, member: &jsx::MemberExpression<Loc, Loc>) -> Printed {
        self.at(&member.loc, |writer| {
            match &member.object {
                jsx::member_expression::Object::Identifier(id) => writer.jsx_identifier(id)?,
                jsx::member_expression::Object::MemberExpression(member) => {
                    writer.jsx_member(member)?
                }
            }
            writer.token(".")?;
            writer.jsx_identifier(&member.property)
        })
    }

    fn jsx_name(&mut self, name: &jsx::Name<Loc, Loc>) -> Printed {
        match name {
            jsx::Name::Identifier(id) => self.jsx_identifier(id),
            jsx::Name::NamespacedName(name) => self.jsx_namespace(name),
            jsx::Name::MemberExpression(member) => self.jsx_member(member),
        }
    }

    fn jsx_container(&mut self, container: &jsx::ExpressionContainer<Loc, Loc>) -> Printed {
        self.token("{")?;
        self.leading(&container.comments)?;
        match &container.expression {
            jsx::expression_container::Expression::Expression(expr) => self.expression(expr)?,
            jsx::expression_container::Expression::EmptyExpression => {}
        }
        self.internal(&container.comments)?;
        self.trailing(&container.comments)?;
        self.token("}")
    }

    pub(super) fn jsx_element(&mut self, element: &jsx::Element<Loc, Loc>) -> Printed {
        self.leading(&element.comments)?;
        let opening = &element.opening_element;
        if opening.targs.is_some() {
            return Err(PrintError::TypeAnnotation(opening.loc.dupe()));
        }
        self.at(&opening.loc, |writer| {
            writer.token("<")?;
            writer.jsx_name(&opening.name)?;
            for attribute in opening.attributes.iter() {
                writer.token(" ")?;
                match attribute {
                    jsx::OpeningAttribute::Attribute(attribute) => {
                        writer.at(&attribute.loc, |writer| {
                            match &attribute.name {
                                jsx::attribute::Name::Identifier(id) => {
                                    writer.jsx_identifier(id)?
                                }
                                jsx::attribute::Name::NamespacedName(name) => {
                                    writer.jsx_namespace(name)?
                                }
                            }
                            if let Some(value) = &attribute.value {
                                writer.token("=")?;
                                match value {
                                    jsx::attribute::Value::StringLiteral((loc, literal)) => {
                                        writer.at(loc, |writer| writer.token(&literal.raw))?
                                    }
                                    jsx::attribute::Value::ExpressionContainer((
                                        loc,
                                        container,
                                    )) => {
                                        writer.at(loc, |writer| writer.jsx_container(container))?
                                    }
                                }
                            }
                            Ok(())
                        })?
                    }
                    jsx::OpeningAttribute::SpreadAttribute(spread) => {
                        writer.at(&spread.loc, |writer| {
                            writer.token("{")?;
                            writer.leading(&spread.comments)?;
                            writer.token("...")?;
                            writer.expression(&spread.argument)?;
                            writer.trailing(&spread.comments)?;
                            writer.token("}")
                        })?
                    }
                }
            }
            writer.token(if opening.self_closing { " />" } else { ">" })
        })?;
        self.jsx_children(&element.children.1)?;
        if let Some(closing) = &element.closing_element {
            self.at(&closing.loc, |writer| {
                writer.token("</")?;
                writer.jsx_name(&closing.name)?;
                writer.token(">")
            })?;
        }
        self.trailing(&element.comments)
    }

    pub(super) fn jsx_fragment(&mut self, fragment: &jsx::Fragment<Loc, Loc>) -> Printed {
        self.leading(&fragment.frag_comments)?;
        self.at(&fragment.frag_opening_element, |writer| writer.token("<>"))?;
        self.jsx_children(&fragment.frag_children.1)?;
        self.at(&fragment.frag_closing_element, |writer| writer.token("</>"))?;
        self.trailing(&fragment.frag_comments)
    }

    fn jsx_children(&mut self, children: &[jsx::Child<Loc, Loc>]) -> Printed {
        for child in children {
            self.at(child.loc(), |writer| match child {
                jsx::Child::Element { inner, .. } => writer.jsx_element(inner),
                jsx::Child::Fragment { inner, .. } => writer.jsx_fragment(inner),
                jsx::Child::ExpressionContainer { inner, .. } => writer.jsx_container(inner),
                jsx::Child::SpreadChild { inner, .. } => {
                    writer.token("{")?;
                    writer.leading(&inner.comments)?;
                    writer.token("...")?;
                    writer.expression(&inner.expression)?;
                    writer.trailing(&inner.comments)?;
                    writer.token("}")
                }
                jsx::Child::Text { inner, .. } => writer.token(&inner.raw),
            })?;
        }
        Ok(())
    }
}
