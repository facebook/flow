(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

type 'loc explanation =
  | ExplanationAbstractEnumCasting
  | ExplanationAlreadyPrinted of Loc.t Flow_errors_utils.Friendly.message_feature list
  | ExplanationArrayInvariantTyping
  | ExplanationConstrainedAssign of {
      name: string;
      declaration: 'loc;
      providers: 'loc list;
      array: bool;
    }
  | ExplanationConcreteEnumCasting of {
      representation_type: string;
      casting_syntax: Options.CastingSyntax.t;
    }
  | ExplanationMultiplatform
  | ExplanationPropertyInvariantTyping
  | ExplanationReactComponentPropsDeepReadOnly of 'loc
  | ExplanationReactComponentRefRequirement
  | ExplanationReactHookArgsDeepReadOnly of 'loc
  | ExplanationReactHookReturnDeepReadOnly of 'loc
  | ExplanationRenderTypeRequirement
  | ExplanationTypeGuardCompatibility

type 'loc frame =
  | FrameAnonymous
  | FrameArrayElement
  | FrameCallableSignature
  | FrameEnumRepresentationType
  | FrameFunNthArgument of int
  | FrameFunThisArgument
  | FrameFunNthParam of int
  | FrameFunThisParam
  | FrameIndexerProperty
  | FrameIndexerPropertyKey
  | FrameInferredTypeForTypeGuardParameter of {
      param: 'loc virtual_reason;
      is_return_false_statement: bool;
    }
  | FrameProperty of name Nel.t
  | FrameTupleIndex of int
  | FrameTypeArgument of 'loc virtual_reason
  | FrameTypeParameterBound of string
  | FrameTypePredicate
  | FrameReturnValue

type 'loc root_message =
  | RootCannotAccessIndex of {
      index: 'loc virtual_reason_desc;
      object_: 'loc virtual_reason_desc;
    }
  | RootCannotAdd of {
      left: 'loc virtual_reason_desc;
      right: 'loc virtual_reason_desc;
    }
  | RootCannotAssign of {
      init: 'loc virtual_reason_desc;
      target: 'loc virtual_reason_desc option;
    }
  | RootCannotCall of 'loc virtual_reason_desc
  | RootCannotCallWithNamedParam of {
      fn: 'loc virtual_reason_desc;
      lower: 'loc virtual_reason_desc;
      name: string;
    }
  | RootCannotCallWithNthParam of {
      fn: 'loc virtual_reason_desc;
      lower: 'loc virtual_reason_desc;
      n: int;
    }
  | RootCannotCallObjectAssign of 'loc virtual_reason_desc
  | RootCannotCast of {
      lower: 'loc virtual_reason_desc;
      upper: 'loc virtual_reason_desc;
    }
  | RootCannotCheckAgainst of {
      test: 'loc virtual_reason_desc;
      discriminant: 'loc virtual_reason;
    }
  | RootCannotCoerce of {
      from: 'loc virtual_reason_desc;
      target: 'loc virtual_reason_desc;
    }
  | RootCannotConformToCommonInterface
  | RootCannotCompareWithProperty of {
      sentinel: 'loc virtual_reason;
      obj: 'loc virtual_reason;
      key: string;
    }
  | RootCannotCreateElement of 'loc virtual_reason_desc
  | RootCannotDeclareRef
  | RootCannotDeclarePredicate of {
      predicate_loc: 'loc;
      fn: 'loc virtual_reason;
    }
  | RootCannotDefineClassMethod of {
      method_: 'loc virtual_reason;
      name: 'loc virtual_reason_desc;
    }
  | RootCannotDefineShadowedProtoProperty
  | RootCannotDelete of 'loc virtual_reason_desc
  | RootCannotExpectImplicitReturn of {
      upper: 'loc virtual_reason_desc;
      fn: 'loc virtual_reason_desc;
    }
  | RootCannotExtendClass of {
      extends: 'loc virtual_reason;
      def: 'loc virtual_reason_desc;
    }
  | RootCannotGetProp of 'loc virtual_reason_desc
  | RootCannotGetRest of 'loc virtual_reason_desc
  | RootCannotImplementClass of {
      implements: 'loc virtual_reason;
      def: 'loc virtual_reason_desc;
    }
  | RootCannotInitializeField of {
      field: 'loc virtual_reason_desc;
      body: 'loc virtual_reason_desc;
    }
  | RootCannotInstantiateEval of 'loc virtual_reason
  | RootCannotInstantiateRenderType of 'loc virtual_reason
  | RootCannotInstantiateTypeApp of 'loc virtual_reason_desc
  | RootCannotReturn of 'loc virtual_reason_desc
  | RootCannotShadowProto of 'loc virtual_reason
  | RootCannotShadowProtoProperty
  | RootCannotSpread of 'loc virtual_reason_desc
  | RootCannotUpdate of 'loc virtual_reason_desc
  | RootCannotUseInferTypeBound of {
      bound: 'loc virtual_reason_desc;
      infer: 'loc virtual_reason_desc;
    }
  | RootCannotUseTypeGuard of {
      guard_type: 'loc virtual_reason;
      param_name: string;
    }
  | RootCannotYield of 'loc virtual_reason_desc

type 'loc message =
  | MessageAlreadyFriendlyPrinted of Loc.t Flow_errors_utils.Friendly.message_feature list
  | MessageCannotAssignToOptionalTupleElement of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageDoesNotRender of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageDollarCallArity of {
      op: 'loc virtual_reason;
      def: 'loc virtual_reason;
      n: int;
    }
  | MessageDollarObjMapArity of {
      op: 'loc virtual_reason;
      value: 'loc virtual_reason;
      def: 'loc virtual_reason;
    }
  | MessageDollarObjMapiArity of {
      op: 'loc virtual_reason;
      key: 'loc virtual_reason;
      value: 'loc virtual_reason;
      def: 'loc virtual_reason;
    }
  | MessageDollarTupleMapArity of {
      op: 'loc virtual_reason;
      value: 'loc virtual_reason;
      def: 'loc virtual_reason;
    }
  | MessageFunctionRequiresAnotherArgument of {
      def: 'loc virtual_reason;
      from: 'loc virtual_reason option;
    }
  | MessageIncompatibleImplicitReturn of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      return: 'loc virtual_reason;
    }
  | MessageIncompatibleComponentRestParam of 'loc virtual_reason
  | MessageIncompatibleGeneral of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatibleMappedTypeKey of {
      source_type: 'loc virtual_reason;
      mapped_type: 'loc virtual_reason;
    }
  | MessageInvalidArgument of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageLowerIsNot of {
      lower: 'loc virtual_reason;
      desc: string;
    }
  | MessageNonLiteralString of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      n: int;
    }
  | MessagePropMissing of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason option;
      prop: string option;
      suggestion: string option;
    }
  | MessagePropPolarityMismatch of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      lpole: Polarity.t;
      upole: Polarity.t;
      prop: string option;
    }
  | MessageShouldNotBeCoerced of 'loc virtual_reason
  | MessageUnknownParameterTypes of 'loc virtual_reason

type 'loc intermediate_error = {
  kind: Flow_errors_utils.error_kind;
  trace_reasons: 'loc virtual_reason list option;
  loc: Loc.t;
  error_code: Error_codes.error_code option;
  root: (Loc.t * 'loc root_message) option;
  message: 'loc error_message;
  misplaced_source_file: File_key.t option;
}

and 'loc error_message =
  | SingletonMessage of {
      message: 'loc message;
      frames: 'loc frame list option;
      explanations: 'loc explanation list option;
    }
  | SpeculationMessage of {
      frames: 'loc frame list;
      explanations: 'loc explanation list;
      branches: (int * 'loc intermediate_error) list;
    }
