// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use dupe::Dupe;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::ast::expression::{self};
use flow_parser::ast::statement::ImportPhase;
use flow_parser::loc::Loc;

use super::Expr;
use super::PrintError;
use super::Printed;
use super::Writer;

impl Writer {
    pub(super) fn expression(&mut self, expression: &Expr) -> Printed {
        self.at(expression.loc(), |writer| match &**expression {
            ExpressionInner::Identifier { inner, .. } => writer.identifier(inner),
            ExpressionInner::StringLiteral { inner, .. } => writer.string(inner),
            ExpressionInner::NumberLiteral { inner, .. } => {
                writer.leading(&inner.comments)?;
                writer.token(&inner.raw)?;
                writer.trailing(&inner.comments)
            }
            ExpressionInner::BigIntLiteral { inner, .. } => {
                writer.leading(&inner.comments)?;
                writer.token(&inner.raw)?;
                writer.trailing(&inner.comments)
            }
            ExpressionInner::BooleanLiteral { inner, .. } => {
                writer.leading(&inner.comments)?;
                writer.token(if inner.value { "true" } else { "false" })?;
                writer.trailing(&inner.comments)
            }
            ExpressionInner::NullLiteral { inner, .. } => {
                writer.leading(inner)?;
                writer.token("null")?;
                writer.trailing(inner)
            }
            ExpressionInner::RegExpLiteral { inner, .. } => {
                writer.leading(&inner.comments)?;
                writer.token(&inner.raw)?;
                writer.trailing(&inner.comments)
            }
            ExpressionInner::This { inner, .. } => {
                writer.leading(&inner.comments)?;
                writer.token("this")?;
                writer.trailing(&inner.comments)
            }
            ExpressionInner::Super { inner, .. } => {
                writer.leading(&inner.comments)?;
                writer.token("super")?;
                writer.trailing(&inner.comments)
            }
            ExpressionInner::AsConstExpression { .. }
            | ExpressionInner::AsExpression { .. }
            | ExpressionInner::TypeCast { .. }
            | ExpressionInner::TSSatisfies { .. }
            | ExpressionInner::ModuleRefLiteral { .. }
            | ExpressionInner::Match { .. }
            | ExpressionInner::Record { .. } => {
                Err(PrintError::UnloweredExpression(expression.loc().dupe()))
            }
            ExpressionInner::Array { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.token("[")?;
                for (index, element) in inner.elements.iter().enumerate() {
                    if index != 0 {
                        writer.token(", ")?;
                    }
                    match element {
                        expression::ArrayElement::Expression(expr) => writer.expression(expr)?,
                        expression::ArrayElement::Spread(spread) => writer.spread(spread)?,
                        expression::ArrayElement::Hole(_) => {}
                    }
                }
                if matches!(
                    inner.elements.last(),
                    Some(expression::ArrayElement::Hole(_))
                ) {
                    writer.token(",")?;
                }
                writer.internal(&inner.comments)?;
                writer.token("]")?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Object { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.token("{")?;
                if !inner.properties.is_empty() {
                    writer.newline();
                    writer.indent += 1;
                    for (index, property) in inner.properties.iter().enumerate() {
                        if index != 0 {
                            writer.token(",")?;
                            writer.newline();
                        }
                        writer.object_property(property)?;
                    }
                    writer.newline();
                    writer.indent -= 1;
                }
                writer.internal(&inner.comments)?;
                writer.token("}")?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Function { inner, .. } => {
                writer.token("(")?;
                writer.function(inner)?;
                writer.token(")")
            }
            ExpressionInner::ArrowFunction { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.function_annotations(inner)?;
                if inner.async_ {
                    writer.token("async ")?;
                }
                writer.params(&inner.params)?;
                writer.token(" => ")?;
                writer.function_body(inner, true)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Class { inner, .. } => {
                writer.token("(")?;
                writer.class(inner)?;
                writer.token(")")
            }
            ExpressionInner::Assignment { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.pattern(&inner.left)?;
                writer.token(" ")?;
                writer.token(inner.operator.as_ref().map_or("=", |op| op.as_str()))?;
                writer.token(" ")?;
                writer.expression(&inner.right)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Binary { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.expression(&inner.left)?;
                writer.token(" ")?;
                writer.token(inner.operator.as_str())?;
                writer.token(" ")?;
                writer.expression(&inner.right)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Logical { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.expression(&inner.left)?;
                writer.token(match inner.operator {
                    expression::LogicalOperator::Or => " || ",
                    expression::LogicalOperator::And => " && ",
                    expression::LogicalOperator::NullishCoalesce => " ?? ",
                })?;
                writer.expression(&inner.right)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Conditional { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.expression(&inner.test)?;
                writer.token(" ? ")?;
                writer.expression(&inner.consequent)?;
                writer.token(" : ")?;
                writer.expression(&inner.alternate)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Sequence { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                for (index, expr) in inner.expressions.iter().enumerate() {
                    if index != 0 {
                        writer.token(", ")?;
                    }
                    writer.expression(expr)?;
                }
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Unary { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.token(match inner.operator {
                    expression::UnaryOperator::Minus => "-",
                    expression::UnaryOperator::Plus => "+",
                    expression::UnaryOperator::Not => "!",
                    expression::UnaryOperator::BitNot => "~",
                    expression::UnaryOperator::Typeof => "typeof",
                    expression::UnaryOperator::Void => "void",
                    expression::UnaryOperator::Delete => "delete",
                    expression::UnaryOperator::Await => "await",
                    expression::UnaryOperator::Nonnull => {
                        return Err(PrintError::UnloweredExpression(expression.loc().dupe()));
                    }
                })?;
                writer.token(" ")?;
                writer.fenced_expression(&inner.argument)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Update { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                let operator = match inner.operator {
                    expression::UpdateOperator::Increment => "++",
                    expression::UpdateOperator::Decrement => "--",
                };
                if inner.prefix {
                    writer.token(operator)?;
                }
                writer.fenced_expression(&inner.argument)?;
                if !inner.prefix {
                    writer.token(operator)?;
                }
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Yield { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.token(if inner.delegate { "yield*" } else { "yield" })?;
                if let Some(argument) = &inner.argument {
                    writer.token(" ")?;
                    writer.fenced_expression(argument)?;
                }
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Member { inner, .. } => {
                writer.token("(")?;
                writer.member(inner, false, false)?;
                writer.token(")")
            }
            ExpressionInner::Call { inner, .. } => {
                writer.token("(")?;
                writer.call(inner, false, false)?;
                writer.token(")")
            }
            ExpressionInner::OptionalMember { .. } | ExpressionInner::OptionalCall { .. } => {
                writer.token("(")?;
                writer.chain(expression)?;
                writer.token(")")
            }
            ExpressionInner::New { inner, .. } => {
                if inner.targs.is_some() {
                    return Err(PrintError::TypeAnnotation(expression.loc().dupe()));
                }
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.token("new ")?;
                writer.fenced_expression(&inner.callee)?;
                match &inner.arguments {
                    Some(arguments) => writer.arguments(arguments)?,
                    None => writer.token("()")?,
                }
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::MetaProperty { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.identifier(&inner.meta)?;
                writer.token(".")?;
                writer.identifier(&inner.property)?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::Import { inner, .. } => {
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.token("import")?;
                if let Some(ImportPhase::Defer) = inner.phase {
                    writer.token(".defer")?;
                }
                writer.token("(")?;
                writer.expression(&inner.argument)?;
                if let Some(options) = &inner.options {
                    writer.token(", ")?;
                    writer.expression(options)?;
                }
                writer.token(")")?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::TemplateLiteral { inner, .. } => writer.template(inner),
            ExpressionInner::TaggedTemplate { inner, .. } => {
                if inner.targs.is_some() {
                    return Err(PrintError::TypeAnnotation(expression.loc().dupe()));
                }
                writer.token("(")?;
                writer.leading(&inner.comments)?;
                writer.fenced_expression(&inner.tag)?;
                writer.at(&inner.quasi.0, |writer| writer.template(&inner.quasi.1))?;
                writer.trailing(&inner.comments)?;
                writer.token(")")
            }
            ExpressionInner::JSXElement { inner, .. } => writer.jsx_element(inner),
            ExpressionInner::JSXFragment { inner, .. } => writer.jsx_fragment(inner),
        })
    }

    pub(super) fn fenced_expression(&mut self, expr: &Expr) -> Printed {
        if matches!(&**expr, ExpressionInner::Super { .. }) {
            return self.expression(expr);
        }
        self.token("(")?;
        self.expression(expr)?;
        self.token(")")
    }

    fn spread(&mut self, spread: &expression::SpreadElement<Loc, Loc>) -> Printed {
        self.at(&spread.loc, |writer| {
            writer.leading(&spread.comments)?;
            writer.token("...")?;
            writer.expression(&spread.argument)?;
            writer.trailing(&spread.comments)
        })
    }

    fn arguments(&mut self, arguments: &expression::ArgList<Loc, Loc>) -> Printed {
        self.at(&arguments.loc, |writer| {
            writer.leading(&arguments.comments)?;
            writer.token("(")?;
            for (index, argument) in arguments.arguments.iter().enumerate() {
                if index != 0 {
                    writer.token(", ")?;
                }
                match argument {
                    expression::ExpressionOrSpread::Expression(expr) => writer.expression(expr)?,
                    expression::ExpressionOrSpread::Spread(spread) => writer.spread(spread)?,
                }
            }
            writer.internal(&arguments.comments)?;
            writer.token(")")?;
            writer.trailing(&arguments.comments)
        })
    }

    fn chain(&mut self, expr: &Expr) -> Printed {
        self.at(expr.loc(), |writer| match &**expr {
            ExpressionInner::OptionalMember { inner, .. } => match inner.optional {
                expression::OptionalMemberKind::Optional => {
                    writer.member(&inner.member, true, true)
                }
                expression::OptionalMemberKind::NonOptional => {
                    writer.member(&inner.member, false, true)
                }
                expression::OptionalMemberKind::AssertNonnull => {
                    Err(PrintError::UnloweredExpression(expr.loc().dupe()))
                }
            },
            ExpressionInner::OptionalCall { inner, .. } => match inner.optional {
                expression::OptionalCallKind::Optional => writer.call(&inner.call, true, true),
                expression::OptionalCallKind::NonOptional => writer.call(&inner.call, false, true),
                expression::OptionalCallKind::AssertNonnull => {
                    Err(PrintError::UnloweredExpression(expr.loc().dupe()))
                }
            },
            _ => writer.fenced_expression(expr),
        })
    }

    fn member(
        &mut self,
        member: &expression::Member<Loc, Loc>,
        optional: bool,
        chain: bool,
    ) -> Printed {
        self.leading(&member.comments)?;
        if chain {
            self.chain(&member.object)?;
        } else {
            self.fenced_expression(&member.object)?;
        }
        if optional {
            self.token("?.")?;
        }
        match &member.property {
            expression::member::Property::PropertyIdentifier(id) => {
                if !optional {
                    self.token(".")?;
                }
                self.identifier(id)?;
            }
            expression::member::Property::PropertyPrivateName(name) => {
                if !optional {
                    self.token(".")?;
                }
                self.private_name(name)?;
            }
            expression::member::Property::PropertyExpression(expr) => {
                self.token("[")?;
                self.expression(expr)?;
                self.token("]")?;
            }
        }
        self.trailing(&member.comments)
    }

    fn call(&mut self, call: &expression::Call<Loc, Loc>, optional: bool, chain: bool) -> Printed {
        if call.targs.is_some() {
            return Err(PrintError::TypeAnnotation(self.location.dupe()));
        }
        self.leading(&call.comments)?;
        if chain {
            self.chain(&call.callee)?;
        } else {
            self.fenced_expression(&call.callee)?;
        }
        if optional {
            self.token("?.")?;
        }
        self.arguments(&call.arguments)?;
        self.trailing(&call.comments)
    }

    fn template(&mut self, template: &expression::TemplateLiteral<Loc, Loc>) -> Printed {
        if template.quasis.len() != template.expressions.len() + 1 {
            return Err(PrintError::TemplateShape(self.location.dupe()));
        }
        self.leading(&template.comments)?;
        self.token("`")?;
        for (index, quasi) in template.quasis.iter().enumerate() {
            self.at(&quasi.loc, |writer| writer.token(&quasi.value.raw))?;
            if let Some(expr) = template.expressions.get(index) {
                self.token("${")?;
                self.expression(expr)?;
                self.token("}")?;
            }
        }
        self.token("`")?;
        self.trailing(&template.comments)
    }

    fn object_property(&mut self, property: &expression::object::Property<Loc, Loc>) -> Printed {
        use expression::object::NormalProperty;
        use expression::object::Property;
        match property {
            Property::SpreadProperty(spread) => self.at(&spread.loc, |writer| {
                writer.leading(&spread.comments)?;
                writer.token("...")?;
                writer.expression(&spread.argument)?;
                writer.trailing(&spread.comments)
            }),
            Property::NormalProperty(property) => {
                self.at(property.loc(), |writer| match property {
                    NormalProperty::Init {
                        key,
                        value,
                        shorthand,
                        ..
                    } => {
                        if *shorthand {
                            writer.expression(value)
                        } else {
                            writer.key(key)?;
                            writer.token(": ")?;
                            writer.expression(value)
                        }
                    }
                    NormalProperty::Method { key, value, .. } => writer.method(key, &value.1, None),
                    NormalProperty::Get {
                        key,
                        value,
                        comments,
                        ..
                    } => {
                        writer.leading(comments)?;
                        writer.method(key, &value.1, Some("get"))?;
                        writer.trailing(comments)
                    }
                    NormalProperty::Set {
                        key,
                        value,
                        comments,
                        ..
                    } => {
                        writer.leading(comments)?;
                        writer.method(key, &value.1, Some("set"))?;
                        writer.trailing(comments)
                    }
                })
            }
        }
    }

    pub(super) fn key(&mut self, key: &expression::object::Key<Loc, Loc>) -> Printed {
        use expression::object::Key;
        match key {
            Key::Identifier(id) => self.identifier(id),
            Key::PrivateName(name) => self.private_name(name),
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

    pub(super) fn computed_key(
        &mut self,
        key: &flow_parser::ast::ComputedKey<Loc, Loc>,
    ) -> Printed {
        self.at(&key.loc, |writer| {
            writer.leading(&key.comments)?;
            writer.token("[")?;
            writer.expression(&key.expression)?;
            writer.token("]")?;
            writer.trailing(&key.comments)
        })
    }
}
