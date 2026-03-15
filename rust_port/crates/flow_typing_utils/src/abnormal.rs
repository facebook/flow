/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_parser::ast::expression::Expression as AstExpression;
use flow_parser::ast::statement;
use flow_parser::ast::statement::Statement;
use flow_parser::ast::statement::StatementInner;
use flow_typing_type::type_::Type;

#[derive(Debug, Clone)]
pub struct AbnormalControlFlow(pub ALoc, pub AstExpression<ALoc, (ALoc, Type)>);

fn catch_control_flow_exception<T>(
    p: impl FnOnce(AbnormalControlFlow) -> T,
    f: impl FnOnce() -> Result<T, AbnormalControlFlow>,
) -> (T, bool) {
    match f() {
        Ok(v) => (v, false),
        Err(e) => (p(e), true),
    }
}

#[allow(clippy::arc_with_non_send_sync)]
pub fn catch_stmt_control_flow_exception<F>(f: F) -> (Statement<ALoc, (ALoc, Type)>, bool)
where
    F: FnOnce() -> Result<Statement<ALoc, (ALoc, Type)>, AbnormalControlFlow>,
{
    catch_control_flow_exception(
        |AbnormalControlFlow(loc, e)| {
            Statement::new(StatementInner::Expression {
                loc,
                inner: std::sync::Arc::new(statement::Expression {
                    expression: e,
                    directive: None,
                    comments: None,
                }),
            })
        },
        f,
    )
}

pub fn catch_expr_control_flow_exception(
    f: impl FnOnce() -> Result<AstExpression<ALoc, (ALoc, Type)>, AbnormalControlFlow>,
) -> (AstExpression<ALoc, (ALoc, Type)>, bool) {
    catch_control_flow_exception(|AbnormalControlFlow(_, e)| e, f)
}

pub fn try_with_abnormal_exn<T>(
    f: impl FnOnce() -> Result<T, AbnormalControlFlow>,
    on_abnormal_exn: impl FnOnce(AbnormalControlFlow) -> T,
) -> T {
    match f() {
        Ok(result) => result,
        Err(e) => on_abnormal_exn(e),
    }
}
