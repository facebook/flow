/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_parser::parse_error::ParseError;
use flow_parsing::docblock_parser::DocblockError;
use flow_parsing::docblock_parser::DocblockErrorKind;
use flow_type_sig::signature_error::TolerableError;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::flow_error;
use flow_typing_errors::flow_error::ErrorSet;
use flow_typing_errors::flow_error::FlowError;
use flow_typing_errors::intermediate_error_types::DocblockError as IntermediateDocblockError;

pub fn error_of_docblock_error(
    source_file: FileKey,
    docblock_error: &DocblockError,
) -> FlowError<ALoc> {
    let DocblockError { loc, kind } = docblock_error;
    let flow_err = ErrorMessage::EDocblockError(Box::new((
        ALoc::of_loc(loc.dupe()),
        match kind {
            DocblockErrorKind::MultipleFlowAttributes => {
                IntermediateDocblockError::MultipleFlowAttributes
            }
            DocblockErrorKind::InvalidFlowMode(s) => {
                IntermediateDocblockError::InvalidFlowMode(FlowSmolStr::from(s.as_str()))
            }
            DocblockErrorKind::MultipleJSXAttributes => {
                IntermediateDocblockError::MultipleJSXAttributes
            }
            DocblockErrorKind::InvalidJSXAttribute(first_error) => {
                IntermediateDocblockError::InvalidJSXAttribute(
                    first_error.as_ref().map(|s| FlowSmolStr::from(s.as_str())),
                )
            }
            DocblockErrorKind::MultipleJSXRuntimeAttributes => {
                IntermediateDocblockError::MultipleJSXRuntimeAttributes
            }
            DocblockErrorKind::InvalidJSXRuntimeAttribute => {
                IntermediateDocblockError::InvalidJSXRuntimeAttribute
            }
            DocblockErrorKind::InvalidSupportsPlatform(p) => {
                IntermediateDocblockError::InvalidSupportsPlatform(FlowSmolStr::from(p.as_str()))
            }
            DocblockErrorKind::DisallowedSupportsPlatform => {
                IntermediateDocblockError::DisallowedSupportsPlatform
            }
        },
    )));
    flow_error::error_of_msg(source_file, flow_err)
}

pub fn set_of_docblock_errors(source_file: FileKey, errors: &[DocblockError]) -> ErrorSet {
    let mut acc = ErrorSet::empty();
    for err in errors {
        acc.add(error_of_docblock_error(source_file.dupe(), err));
    }
    acc
}

pub fn error_of_parse_error(
    source_file: FileKey,
    (loc, err): (Loc, ParseError),
) -> FlowError<ALoc> {
    let flow_err = ErrorMessage::EParseError(Box::new((ALoc::of_loc(loc), err)));
    flow_error::error_of_msg(source_file, flow_err)
}

pub fn set_of_parse_error(source_file: FileKey, parse_error: (Loc, ParseError)) -> ErrorSet {
    ErrorSet::singleton(error_of_parse_error(source_file, parse_error))
}

pub fn error_of_parse_exception(source_file: FileKey, exn: FlowSmolStr) -> FlowError<ALoc> {
    let file_loc = ALoc::of_loc(Loc {
        source: Some(source_file.dupe()),
        ..LOC_NONE
    });
    let flow_err =
        ErrorMessage::EInternal(Box::new((file_loc, InternalError::ParseJobException(exn))));
    flow_error::error_of_msg(source_file, flow_err)
}

pub fn set_of_parse_exception(source_file: FileKey, exn: FlowSmolStr) -> ErrorSet {
    ErrorSet::singleton(error_of_parse_exception(source_file, exn))
}

fn error_of_file_sig_tolerable_error(
    source_file: FileKey,
    err: &TolerableError<Loc>,
) -> FlowError<ALoc> {
    let flow_err = match err {
        TolerableError::SignatureVerificationError(sve) => ErrorMessage::ESignatureVerification(
            sve.map(&mut (), |_, loc| ALoc::of_loc(loc.dupe())),
        ),
        TolerableError::SignatureBindingValidationError(sve) => {
            ErrorMessage::ESignatureBindingValidation(
                sve.map(&mut (), |_, loc| ALoc::of_loc(loc.dupe())),
            )
        }
    };
    flow_error::error_of_msg(source_file, flow_err)
}

pub fn set_of_file_sig_tolerable_errors(
    source_file: FileKey,
    errors: &[TolerableError<Loc>],
) -> ErrorSet {
    errors
        .iter()
        .map(|err| error_of_file_sig_tolerable_error(source_file.dupe(), err))
        .collect()
}
