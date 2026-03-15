/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;

use flow_aloc::ALoc;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_type::type_::SpeculationHintState;
use flow_typing_type::type_::Type;

#[derive(Debug, Clone)]
pub enum InformationForSynthesisLogging {
    CallInformationForSynthesisLogging {
        lhs_t: Type,
        call_callee_hint_ref: Rc<RefCell<SpeculationHintState>>,
    },
    NoInformationForSynthesisLogging,
}

// Next, a model for "cases." A case serves as the context for a speculative
// match. In other words, while we're trying to execute a flow in speculation
// mode, we use this data structure to record stuff.
//
// A case carries a (local) index that identifies which type we're currently
// considering among the members of a union or intersection type. This is used
// only for error reporting.
#[derive(Debug, Clone)]
pub struct Case {
    pub case_id: i32,
    pub errors: Rc<RefCell<Vec<ErrorMessage<ALoc>>>>,
    pub information_for_synthesis_logging: InformationForSynthesisLogging,
}

/// A branch is a wrapper around a case, that also carries the speculation id of
/// the spec currently being processed.
#[derive(Debug, Clone)]
pub struct Branch {
    pub speculation_id: i32,
    pub case: Case,
}

pub struct SpeculationState(pub Vec<Branch>);
