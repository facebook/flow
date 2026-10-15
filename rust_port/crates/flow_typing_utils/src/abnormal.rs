/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_parser::ast::expression::Expression as AstExpression;
use flow_parser::ast::statement;
use flow_parser::ast::statement::Statement;
use flow_parser::ast::statement::StatementInner;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::empty_t;
use flow_utils_concurrency::job_error::CheckTimeout;
use flow_utils_concurrency::job_error::JobError;
use flow_utils_concurrency::worker_cancel::WorkerCanceled;

#[derive(Debug, Clone)]
pub struct AbnormalControlFlow(pub ALoc, pub AstExpression<ALoc, (ALoc, Type)>);

#[derive(Debug, Clone)]
pub enum CheckExprError {
    Abnormal(AbnormalControlFlow),
    Canceled(WorkerCanceled),
    TimedOut(CheckTimeout),
    DebugThrow { loc: ALoc },
}

impl From<AbnormalControlFlow> for CheckExprError {
    fn from(e: AbnormalControlFlow) -> Self {
        CheckExprError::Abnormal(e)
    }
}

impl From<WorkerCanceled> for CheckExprError {
    fn from(e: WorkerCanceled) -> Self {
        CheckExprError::Canceled(e)
    }
}

impl From<CheckTimeout> for CheckExprError {
    fn from(e: CheckTimeout) -> Self {
        CheckExprError::TimedOut(e)
    }
}

impl From<JobError> for CheckExprError {
    fn from(e: JobError) -> Self {
        match e {
            JobError::Canceled(c) => CheckExprError::Canceled(c),
            JobError::TimedOut(t) => CheckExprError::TimedOut(t),
            JobError::DebugThrow { loc } => CheckExprError::DebugThrow { loc },
        }
    }
}

fn catch_control_flow_exception<T>(
    p: impl FnOnce(AbnormalControlFlow) -> T,
    f: impl FnOnce() -> Result<T, CheckExprError>,
) -> Result<(T, bool), JobError> {
    match f() {
        Ok(v) => Ok((v, false)),
        Err(CheckExprError::Abnormal(e)) => Ok((p(e), true)),
        Err(CheckExprError::Canceled(c)) => Err(JobError::Canceled(c)),
        Err(CheckExprError::TimedOut(t)) => Err(JobError::TimedOut(t)),
        Err(CheckExprError::DebugThrow { loc }) => Err(JobError::DebugThrow { loc }),
    }
}

/// Preserve expression and return statements when their expressions exit abnormally.
#[expect(
    clippy::arc_with_non_send_sync,
    reason = "Typed AST nodes share structure within a single checker thread"
)]
pub fn catch_stmt_control_flow_exception<F>(
    stmt: &Statement<ALoc, ALoc>,
    f: F,
) -> Result<(Statement<ALoc, (ALoc, Type)>, bool), JobError>
where
    F: FnOnce() -> Result<Statement<ALoc, (ALoc, Type)>, CheckExprError>,
{
    catch_control_flow_exception(
        |AbnormalControlFlow(loc, e)| {
            if let StatementInner::Return { loc, inner } = &**stmt {
                return Statement::new(StatementInner::Return {
                    loc: loc.dupe(),
                    inner: Arc::new(statement::Return {
                        argument: Some(e),
                        comments: inner.comments.dupe(),
                        return_out: (
                            inner.return_out.dupe(),
                            empty_t::at(inner.return_out.dupe()),
                        ),
                    }),
                });
            }
            let (loc, directive, comments) = match &**stmt {
                StatementInner::Expression { loc, inner } => {
                    (loc.dupe(), inner.directive.clone(), inner.comments.dupe())
                }
                _ => (loc, None, None),
            };
            Statement::new(StatementInner::Expression {
                loc,
                inner: Arc::new(statement::Expression {
                    expression: e,
                    directive,
                    comments,
                }),
            })
        },
        f,
    )
}

pub fn catch_expr_control_flow_exception(
    f: impl FnOnce() -> Result<AstExpression<ALoc, (ALoc, Type)>, CheckExprError>,
) -> Result<(AstExpression<ALoc, (ALoc, Type)>, bool), JobError> {
    catch_control_flow_exception(|AbnormalControlFlow(_, e)| e, f)
}

pub fn try_with_abnormal_exn<T>(
    f: impl FnOnce() -> Result<T, CheckExprError>,
    on_abnormal_exn: impl FnOnce(AbnormalControlFlow) -> T,
) -> Result<T, JobError> {
    match f() {
        Ok(result) => Ok(result),
        Err(CheckExprError::Abnormal(e)) => Ok(on_abnormal_exn(e)),
        Err(CheckExprError::Canceled(c)) => Err(JobError::Canceled(c)),
        Err(CheckExprError::TimedOut(t)) => Err(JobError::TimedOut(t)),
        Err(CheckExprError::DebugThrow { loc }) => Err(JobError::DebugThrow { loc }),
    }
}
