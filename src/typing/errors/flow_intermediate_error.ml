(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Utils_js
open Reason
open Error_message

exception ImproperlyFormattedError of Loc.t Error_message.t'

(* Rank scores for signals of different strength on an x^2 scale so that greater
 * signals dominate lesser signals. *)
let reason_score = 100

let frame_score = reason_score * 2

let type_arg_frame_score = frame_score * 2

let tuple_element_frame_score = type_arg_frame_score * 2

(* Gets the score of a use_op. Used in score_of_msg. See the comment on
 * score_of_msg to learn more about scores.
 *
 * Calculated by taking the count of all the frames. *)
let score_of_use_op use_op =
  let score =
    fold_use_op
      (* Comparing the scores of use_ops only works when they all have the same
       * root_use_op! If two use_ops have different roots, we can't realistically
       * compare the number of frames since the basis is completely different.
       *
       * So we require a Speculation root use_op to be passed into score_of_use_op
       * and we perform a structural equality check using that.
       *
       * Otherwise, the total score from score_of_use_op is -1. This way, errors
       * which match Speculation will be promoted. It is more likely the user was
       * trying to target these branches. *)
        (function
        | Type.Speculation _ -> Ok 0
        | _ -> Error (-1))
      (fun acc frame ->
        match acc with
        | Error _ -> acc
        | Ok acc ->
          Ok
            (acc
            +
            match frame with
            (* Later params that error get a higher score. This roughly represents how
             * much type-checking work Flow successfully completed before erroring.
             * Useful for basically only overloaded function error messages.
             *
             * The signal that this gives us is that we successfully type checked n
             * params in the call before erroring. If there was no error, Flow may
             * have gone to successfully check another m params. However, we will
             * never know that. n is our best approximation. It rewards errors near
             * the end of a call and punishes (slightly) errors near the beginning of
             * a call.
             *
             * This, however, turns out to be consistent with code style in modern
             * JavaScript. As an unspoken convention, more complex arguments usually
             * go last. For overloaded functions, the switching generally happens on
             * the first argument. The "tag". This gives us confidence that n on
             * FunParam is a good heuristic for the score.
             *
             * FunRestParam is FunParam, but at the end. So give it a larger score
             * then FunParam after adding n.
             *
             * We do _not_ add n to the score if this use_op was added to an implicit type parameter. *)
            | FunParam { n; _ } -> frame_score + n
            | FunRestParam _ -> frame_score + frame_score - 1
            (* FunCompatibility is generally followed by another use_op. So let's not
             * count FunCompatibility. *)
            | FunCompatibility _ -> 0
            (* FunMissingArg means the error is *less* likely to be correct. *)
            | FunMissingArg _ -> 0
            (* Higher signal then PropertyCompatibility, for example. *)
            | TypeArgCompatibility _ -> type_arg_frame_score
            | ArrayElementCompatibility _ -> type_arg_frame_score
            (* Higher signal then TypeArgCompatibility. *)
            | TupleElementCompatibility _ -> tuple_element_frame_score
            (* ImplicitTypeParam is an internal marker use_op that doesn't get
             * rendered in error messages. So it doesn't necessarily signal anything
             * about the user's intent. *)
            | ImplicitTypeParam -> 0
            | _ -> frame_score
            ))
      use_op
  in
  match score with
  | Ok n -> n
  | Error n -> n

(* Gets the score of an error message. The score is an approximation of how
 * close the user was to getting their code right. A higher score means the user
 * was closer then a lower score. A score of 0 means we have no signal about
 * how close the user was. For example, consider the following two flows:
 *
 *     number ~> {p: string}
 *
 *     {p: number} ~> {p: string}
 *
 * Clearly, the user was closer to being correct with the second flow. So this
 * function should assign the number ~> string error a higher score then the
 * number ~> object error.
 *
 * Now consider:
 *
 *     number ~> string
 *
 *     number ~> {p: string}
 *
 * This time we kept the lower bound the same and changed the upper bound. The
 * first flow is this time is closer to the user's intent then the second flow.
 * So we give the number ~> string message a higher score then the
 * number ~> object message.
 *
 * This scoring mechanism is useful for union and intersection error messages
 * where we want to approximate which branch the user meant to target with
 * their code. Branches with higher scores have a higher liklihood of being
 * the branch the user was targeting. *)
let score_of_msg msg =
  (* Start by getting the score based off the use_op of our error message. If
   * the message does not have a use_op then we return 0. This score
   * contribution declares that greater complexity in the use is more likely to
   * cause a match. *)
  let score = util_use_op_of_msg 0 (fun op _ -> score_of_use_op op) msg in
  (* Special cases for messages which increment the score. *)
  let score =
    score
    +
    match msg with
    (* If a property doesn't exist, we still use a PropertyCompatibility use_op.
     * This PropertyCompatibility when counted in our score is dishonest since
     * a missing prop does not increase the likelihood that the user was close to
     * the right types. *)
    | EIncompatibleProp { use_op = Some (Frame (PropertyCompatibility _, _)); _ }
    | EPropNotFound { use_op = Frame (PropertyCompatibility _, _); _ } ->
      -frame_score
    | _ -> 0
  in
  (* If we have two incompatible types and both incompatible types are scalar or
   * both types are arrays then increment our score. This is based on the belief
   * that the solutions with the lowest possible complexity are closest to each
   * other. e.g. number ~> string. If one type is a scalar or array and the
   * other type is not then we decrement our score. *)
  let score =
    score
    +
    let reasons =
      match msg with
      | EIncompatibleDefs { reason_lower = rl; reason_upper = ru; branches = []; use_op = _ }
      | EIncompatibleWithUseOp { reason_lower = rl; reason_upper = ru; _ }
      | EIncompatibleWithExact ((rl, ru), _, _) ->
        Some (rl, ru)
      | _ -> None
    in
    match reasons with
    | Some (rl, ru) ->
      if is_nullish_reason rl && is_nullish_reason ru then
        reason_score
      else if
        (* T ~> null should have a lower score then T ~> scalar *)
        is_nullish_reason rl || is_nullish_reason ru
      then
        0
      else if is_scalar_reason rl && is_scalar_reason ru then
        reason_score
      else if is_scalar_reason rl || is_scalar_reason ru then
        1
      else if is_array_reason rl && is_array_reason ru then
        reason_score
      else if is_array_reason rl || is_array_reason ru then
        1
      else
        reason_score
    | None -> reason_score
  in
  score

(* Flip the lower/upper reasons of a frame_use_op. *)
let flip_frame = function
  | ArrayElementCompatibility c -> ArrayElementCompatibility { lower = c.upper; upper = c.lower }
  | FunCompatibility c -> FunCompatibility { lower = c.upper; upper = c.lower }
  | FunParam c -> FunParam { c with lower = c.upper; upper = c.lower }
  | FunRestParam c -> FunRestParam { lower = c.upper; upper = c.lower }
  | FunReturn c -> FunReturn { lower = c.upper; upper = c.lower }
  | IndexerKeyCompatibility c -> IndexerKeyCompatibility { lower = c.upper; upper = c.lower }
  | OpaqueTypeSuperCompatibility c ->
    OpaqueTypeSuperCompatibility { lower = c.upper; upper = c.lower }
  | PropertyCompatibility c -> PropertyCompatibility { c with lower = c.upper; upper = c.lower }
  | ReactConfigCheck -> ReactConfigCheck
  | TupleElementCompatibility c ->
    TupleElementCompatibility
      {
        c with
        lower = c.upper;
        upper = c.lower;
        lower_optional = c.upper_optional;
        upper_optional = c.lower_optional;
      }
  | TypeArgCompatibility c -> TypeArgCompatibility { c with lower = c.upper; upper = c.lower }
  | EnumRepresentationTypeCompatibility c ->
    EnumRepresentationTypeCompatibility { lower = c.upper; upper = c.lower }
  | ( CallFunCompatibility _ | TupleMapFunCompatibility _ | TupleAssignment _
    | ObjMapFunCompatibility _ | ObjMapiFunCompatibility _ | TypeParamBound _ | OpaqueTypeBound _
    | FunMissingArg _ | ImplicitTypeParam | ReactGetConfig _ | UnifyFlip | ConstrainedAssignment _
    | MappedTypeKeyCompatibility _ | TypePredicateCompatibility
    | InferredTypeForTypeGuardParameter _ | RendersCompatibility | ReactDeepReadOnly _ ) as use_op
    ->
    use_op

let post_process_errors original_errors =
  (* Unification produces two errors. One for both sides. For example,
   * {p: number} ~> {p: string} errors on both number ~> string and
   * string ~> number. Showing both errors to our user is often redundant.
   * So we use this utility to flip the string ~> number case and produce an
   * error identical to one we've produced before. These two errors will be
   * deduped by the filter below. *)
  let dedupe_by_flip =
    (* Loop over through the use_op chain. *)
    let rec loop = function
      (* Roots don't flip. *)
      | Op _ as use_op -> (false, use_op)
      (* Start flipping if we are on the reverse side of unification. *)
      | Frame (UnifyFlip, use_op) ->
        let (flip, use_op) = loop use_op in
        (not flip, use_op)
      (* If we are in flip mode then flip our frame. *)
      | Frame (frame, use_op) ->
        let (flip, use_op) = loop use_op in
        if flip then
          (true, Frame (flip_frame frame, use_op))
        else
          (false, Frame (frame, use_op))
    in
    fun (lower, upper) use_op ->
      let (flip, use_op) = loop use_op in
      if flip then
        ((upper, lower), use_op)
      else
        ((lower, upper), use_op)
  in
  let rec remove_unify_flip = function
    | Op _ as use_op -> use_op
    (* Start flipping if we are on the reverse side of unification. *)
    | Frame (UnifyFlip, use_op) -> remove_unify_flip use_op
    | Frame (frame, use_op) -> Frame (frame, remove_unify_flip use_op)
  in
  let retain_error error =
    let open Error_message in
    let is_not_duplicate new_msg =
      let e' =
        let open Flow_error in
        error_of_msg ~trace_reasons:(trace_reasons error) ~source_file:(source_file error) new_msg
      in
      not @@ Flow_error.ErrorSet.mem e' original_errors
    in
    (* The error will be retained, if dedupe_by_flip doesn't change the error,
       or the error is unique even after deduplication. *)
    match Flow_error.msg_of_error error with
    | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate
           (EIncompatibleDefs { use_op; reason_lower = reason_lower'; reason_upper; branches })
    | EExpectedStringLit { reason_lower; reason_upper; use_op } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate (EExpectedStringLit { reason_lower = reason_lower'; reason_upper; use_op })
    | EExpectedNumberLit { reason_lower; reason_upper; use_op } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate (EExpectedNumberLit { reason_lower = reason_lower'; reason_upper; use_op })
    | EExpectedBooleanLit { reason_lower; reason_upper; use_op } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate
           (EExpectedBooleanLit { reason_lower = reason_lower'; reason_upper; use_op })
    | EExpectedBigIntLit { reason_lower; reason_upper; use_op } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate (EExpectedBigIntLit { reason_lower = reason_lower'; reason_upper; use_op })
    | EIncompatibleWithUseOp { reason_lower; reason_upper; use_op } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate
           (EIncompatibleWithUseOp { reason_lower = reason_lower'; reason_upper; use_op })
    | EEnumIncompatible
        { reason_lower; reason_upper; use_op; enum_kind; representation_type; casting_syntax } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate
           (EEnumIncompatible
              {
                reason_lower = reason_lower';
                reason_upper;
                use_op;
                enum_kind;
                representation_type;
                casting_syntax;
              }
           )
    | EPropNotFound { prop_name; reason_obj; reason_prop; use_op; suggestion } ->
      (* PropNotFound error will always display the missing vs existing prop in the same order. *)
      let use_op' = remove_unify_flip use_op in
      use_op = use_op'
      || is_not_duplicate
           (EPropNotFound { prop_name; reason_obj; reason_prop; use_op = use_op'; suggestion })
    | _ -> true
  in
  Flow_error.ErrorSet.filter retain_error original_errors

open Flow_intermediate_error_types

let mk_error
    ?(kind = Flow_errors_utils.InferError)
    ?trace_reasons
    ?root
    ?frames
    ?explanations
    loc
    error_code
    message =
  {
    kind;
    trace_reasons;
    loc;
    root;
    error_code;
    message = SingletonMessage { message; frames; explanations };
    misplaced_source_file = None;
  }

let mk_speculation_error
    ?(kind = Flow_errors_utils.InferError)
    ?trace_reasons
    ~loc
    ~root
    ~frames
    ~explanations
    ~error_code
    speculation_errors =
  {
    kind;
    trace_reasons;
    loc;
    root;
    error_code;
    message = SpeculationMessage { frames; explanations; branches = speculation_errors };
    misplaced_source_file = None;
  }

let rec make_intermediate_error :
          'loc.
          loc_of_aloc:('loc -> Loc.t) ->
          speculation:bool ->
          'loc Flow_error.t ->
          'loc intermediate_error =
 fun ~loc_of_aloc ~speculation error ->
  let loc = Flow_error.loc_of_error error in
  let msg = Flow_error.msg_of_error error in
  let source_file = Flow_error.source_file error in
  let trace_reasons = Flow_error.trace_reasons error in
  let kind = kind_of_msg msg in
  (* In friendly error messages, we always want to point to a value as the
   * primary location. Or an annotation on a value. Normally, values are found
   * in the lower bound. However, in contravariant positions this flips. In this
   * function we normalize the lower/upper variables in use_ops so that lower
   * always points to the value. Example:
   *
   *     ((x: number) => {}: (x: string) => void);
   *
   * We want to point to number. However, number is in the upper position since
   * number => void ~> string => void flips arguments to string ~> number. This
   * function flips contravariant positions like function arguments back. *)
  let flip_contravariant =
    (* Is this frame part of a contravariant position? *)
    let is_contravariant = function
      | (FunParam _, Frame (FunCompatibility _, _)) -> (true, true)
      | (FunRestParam _, Frame (FunCompatibility _, _)) -> (true, true)
      | (ReactGetConfig { polarity = Polarity.Negative }, _) -> (true, false)
      | (TypeArgCompatibility { polarity = Polarity.Negative; _ }, _) -> (true, false)
      | _ -> (false, false)
    in
    let is_contravariant_root = function
      | FunImplicitReturn _ -> true
      | _ -> false
    in
    (* Loop through the use_op and flip the contravariants. *)
    let rec loop = function
      | Op root_use_op as use_op -> (is_contravariant_root root_use_op, use_op)
      (* If the frame is contravariant then flip. *)
      | Frame (frame, use_op) ->
        let (flip, use_op) = loop use_op in
        let (contravariant, flip_self) = is_contravariant (frame, use_op) in
        let flip =
          if contravariant then
            not flip
          else
            flip
        in
        let flip_self = flip && ((not contravariant) || flip_self) in
        let frame =
          if flip_self then
            flip_frame frame
          else
            frame
        in
        (flip, Frame (frame, use_op))
    in
    fun (lower, upper) use_op ->
      let (flip, use_op) = loop use_op in
      if flip then
        ((upper, lower), use_op)
      else
        ((lower, upper), use_op)
  in
  let rec mod_lower_reason_according_to_use_ops lower = function
    | Frame (OpaqueTypeBound { opaque_t_reason }, use_op) ->
      mod_lower_reason_according_to_use_ops opaque_t_reason use_op
    | Frame (_, use_op) -> mod_lower_reason_according_to_use_ops lower use_op
    | Op _ -> lower
  in
  let desc = Reason.desc_of_reason ~unwrap:false in
  (* Unwrap a use_op for the friendly error format. Takes the smallest location
   * where we found the error and a use_op which we will unwrap. *)
  let unwrap_use_ops :
      Loc.t ->
      'loc virtual_use_op ->
      (Loc.t * 'loc root_message) option * Loc.t * 'loc frame list * 'loc explanation list =
    let rec loop loc frames use_op =
      match use_op with
      | Op UnknownUse -> unknown_root loc frames
      | Op (Type.Speculation _) when speculation -> unknown_root loc frames
      | Op (Type.Speculation use) -> loop loc frames use
      | Op (ObjectSpread { op }) -> root loc frames op (RootCannotSpread (desc op))
      | Op (ObjectRest { op }) -> root loc frames op (RootCannotGetRest (desc op))
      | Op (ObjectChain { op }) -> root loc frames op (RootCannotCallObjectAssign (desc op))
      | Op (Arith { op; left; right }) ->
        root loc frames op (RootCannotAdd { left = desc left; right = desc right })
      | Op (AssignVar { var; init }) ->
        root loc frames init (RootCannotAssign { init = desc init; target = Option.map desc var })
      | Op (DeleteVar { var }) -> root loc frames var (RootCannotDelete (desc var))
      | Op (InitField { op; body }) ->
        root loc frames op (RootCannotInitializeField { field = desc op; body = desc body })
      | Op (Cast { lower; upper }) ->
        root loc frames lower (RootCannotCast { lower = desc lower; upper = desc upper })
      | Op (ClassExtendsCheck { extends; def }) ->
        root loc frames def (RootCannotExtendClass { extends; def = desc def })
      | Op (ClassMethodDefinition { name; def }) ->
        root loc frames def (RootCannotDefineClassMethod { method_ = def; name = desc name })
      | Op (ClassImplementsCheck { implements; def; _ }) ->
        root loc frames def (RootCannotImplementClass { implements; def = desc def })
      | Op (ClassOwnProtoCheck { prop; own_loc; proto_loc }) ->
        (match (own_loc, proto_loc) with
        | (None, None) -> unknown_root loc frames
        | (Some loc, None) ->
          let def = mk_reason (RProperty (Some prop)) loc in
          root (loc_of_aloc loc) frames def RootCannotShadowProtoProperty
        | (None, Some loc) ->
          let def = mk_reason (RProperty (Some prop)) loc in
          root (loc_of_aloc loc) frames def RootCannotDefineShadowedProtoProperty
        | (Some own_loc, Some proto_loc) ->
          let def = mk_reason (RProperty (Some prop)) own_loc in
          let proto = mk_reason (RProperty (Some prop)) proto_loc in
          root loc frames def (RootCannotShadowProto proto))
      | Op (Coercion { from; target }) ->
        root loc frames from (RootCannotCoerce { from = desc from; target = desc target })
      | Op (ConformToCommonInterface { self_sig_loc; self_module_loc }) ->
        let frames =
          let (all_frames, explanations) = frames in
          (all_frames, ExplanationMultiplatform :: explanations)
        in
        root_with_loc_and_specific_loc
          loc
          frames
          (loc_of_aloc self_module_loc)
          (loc_of_aloc self_sig_loc)
          RootCannotConformToCommonInterface
      | Op (DeclareComponentRef { op }) ->
        let frames =
          let (all_frames, explanations) = frames in
          (all_frames, ExplanationReactComponentRefRequirement :: explanations)
        in
        root loc frames op RootCannotDeclareRef
      | Op (FunCall { op; fn; _ }) ->
        root_with_specific_reason loc frames op fn (RootCannotCall (desc fn))
      | Op (FunCallMethod { op; fn; prop; _ }) ->
        root_with_specific_reason loc frames op prop (RootCannotCall (desc fn))
      | Op (RenderTypeInstantiation { render_type }) ->
        let frames =
          let (all_frames, explanations) = frames in
          (all_frames, ExplanationRenderTypeRequirement :: explanations)
        in
        root loc frames render_type (RootCannotInstantiateRenderType render_type)
      | Frame
          ( FunParam _,
            (Op (Type.Speculation (Op (FunCall _ | FunCallMethod _ | JSXCreateElement _))) as use_op)
          ) ->
        loop loc frames use_op
      | Frame
          ( FunParam { n; name; lower = lower'; _ },
            Op (FunCall { args; fn; _ } | FunCallMethod { args; fn; _ })
          ) ->
        let lower =
          if List.length args > n - 1 then
            List.nth args (n - 1)
          else
            lower'
        in
        let root_msg =
          match name with
          | Some name -> RootCannotCallWithNamedParam { fn = desc fn; lower = desc lower; name }
          | None -> RootCannotCallWithNthParam { fn = desc fn; lower = desc lower; n }
        in
        root loc frames lower root_msg
      | Op (FunReturnStatement { value }) -> root loc frames value (RootCannotReturn (desc value))
      | Op (FunImplicitReturn { upper; fn; predicate = true }) ->
        root
          loc
          frames
          upper
          (RootCannotDeclarePredicate { predicate_loc = loc_of_reason upper; fn })
      | Op (FunImplicitReturn { upper; fn; _ }) ->
        root loc frames upper (RootCannotExpectImplicitReturn { upper = desc upper; fn = desc fn })
      | Op (GeneratorYield { value }) -> root loc frames value (RootCannotYield (desc value))
      | Op (GetProperty prop) -> root loc frames prop (RootCannotGetProp (desc prop))
      | Op (IndexedTypeAccess { _object; index }) ->
        root loc frames index (RootCannotAccessIndex { index = desc index; object_ = desc _object })
      | Op (InferBoundCompatibilityCheck { bound; infer }) ->
        root
          loc
          frames
          bound
          (RootCannotUseInferTypeBound { bound = desc bound; infer = desc infer })
      | Frame (FunParam _, Op (JSXCreateElement { op; component; _ }))
      | Op (JSXCreateElement { op; component; _ })
      | Op (ReactCreateElementCall { op; component; _ }) ->
        root_with_specific_reason loc frames op component (RootCannotCreateElement (desc component))
      | Op (ReactGetIntrinsic { literal }) ->
        root loc frames literal (RootCannotCreateElement (desc literal))
      | Op (TypeApplication { type_ }) ->
        root loc frames type_ (RootCannotInstantiateTypeApp (desc type_))
      | Op (SetProperty { prop; value; lhs; _ }) ->
        let loc_reason =
          if Loc.contains (loc_of_aloc (loc_of_reason lhs)) loc then
            lhs
          else
            value
        in
        root
          loc
          frames
          loc_reason
          (RootCannotAssign { init = desc value; target = Some (desc prop) })
      | Op (UpdateProperty { prop; lhs }) -> root loc frames lhs (RootCannotUpdate (desc prop))
      | Op (DeleteProperty { prop; lhs }) -> root loc frames lhs (RootCannotDelete (desc prop))
      | Op (RefinementCheck { test; discriminant }) ->
        root loc frames test (RootCannotCheckAgainst { test = desc test; discriminant })
      | Op (MatchingProp { op; obj; key; sentinel_reason }) ->
        root loc frames op (RootCannotCompareWithProperty { sentinel = sentinel_reason; obj; key })
      | Op (EvalMappedType { mapped_type }) ->
        root loc frames mapped_type (RootCannotInstantiateEval mapped_type)
      | Op (TypeGuardIncompatibility { guard_type; param_name }) ->
        let (all_frames, explanations) = frames in
        let frames = (all_frames, ExplanationTypeGuardCompatibility :: explanations) in
        root loc frames guard_type (RootCannotUseTypeGuard { guard_type; param_name })
      | Op (ComponentRestParamCompatibility { rest_param = _ }) ->
        (* Special-cased incompatibility error. This should be unreachable, but just in case
         * our error messages can compose in a way that would miss the special case we'd rather
         * output a bad error than crash. *)
        unknown_root loc frames
      | Frame (ReactDeepReadOnly (props_loc, Props), use_op) ->
        explanation loc frames use_op (ExplanationReactComponentPropsDeepReadOnly props_loc)
      | Frame (ReactDeepReadOnly (props_loc, HookArg), use_op) ->
        explanation loc frames use_op (ExplanationReactHookArgsDeepReadOnly props_loc)
      | Frame (ReactDeepReadOnly (hook_loc, HookReturn), use_op) ->
        explanation loc frames use_op (ExplanationReactHookReturnDeepReadOnly hook_loc)
      | Frame (ConstrainedAssignment { name; declaration; providers; array }, use_op) ->
        explanation
          loc
          frames
          use_op
          (ExplanationConstrainedAssign { name; declaration; providers; array })
      | Frame (UnifyFlip, (Frame (ArrayElementCompatibility _, _) as use_op)) ->
        explanation loc frames use_op ExplanationArrayInvariantTyping
      | Frame (ArrayElementCompatibility { lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op FrameArrayElement
      | Frame (FunParam { n; lower; name; _ }, (Frame (FunCompatibility _, _) as use_op)) ->
        let arg =
          match name with
          | Some "this" -> FrameFunThisParam
          | _ -> FrameFunNthParam n
        in
        unwrap_frame loc frames lower use_op arg
      | Frame (FunParam { n; lower; name; _ }, use_op) ->
        let arg =
          match name with
          | Some "this" -> FrameFunThisArgument
          | _ -> FrameFunNthArgument n
        in
        unwrap_frame loc frames lower use_op arg
      | Frame (FunRestParam _, use_op) -> loop loc frames use_op
      | Frame (FunReturn _, use_op) -> unwrap_frame_without_loc loc frames use_op FrameReturnValue
      | Frame (IndexerKeyCompatibility { lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op FrameIndexerPropertyKey
      | Frame (OpaqueTypeSuperCompatibility { lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op FrameAnonymous
      | Frame (PropertyCompatibility { prop = None; lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op FrameIndexerProperty
      | Frame (PropertyCompatibility { prop = Some (OrdinaryName "$call"); lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op FrameCallableSignature
      | Frame (EnumRepresentationTypeCompatibility { lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op FrameEnumRepresentationType
      | Frame (UnifyFlip, (Frame (PropertyCompatibility _, _) as use_op)) ->
        explanation loc frames use_op ExplanationPropertyInvariantTyping
      | Frame (PropertyCompatibility { prop = Some prop; lower; _ }, use_op) ->
        let loc_of_prop_compatibility_reason loc reason = function
          (* If we are checking class extensions or implementations then the
           * object reason will point to the class name. So don't reposition with
           * this reason. *)
          | Op (ClassExtendsCheck _) -> loc
          | Op (ClassImplementsCheck _) -> loc
          | _ -> loc_of_aloc (loc_of_reason reason)
        in
        let rec loop lower_loc = function
          (* Don't match $call properties since they have special
           * meaning. As defined above. *)
          | Frame (PropertyCompatibility { prop = Some prop; lower = lower'; _ }, use_op)
          (* TODO the $-prefixed names should be internal *)
            when prop <> OrdinaryName "$call" ->
            let lower_loc' = loc_of_prop_compatibility_reason lower_loc lower' use_op in
            (* Perform the same frame location unwrapping as we do in our
             * general code. *)
            let lower_loc =
              if Loc.contains lower_loc' lower_loc then
                lower_loc
              else
                lower_loc'
            in
            let (lower_loc, props, use_op) = loop lower_loc use_op in
            (lower_loc, prop :: props, use_op)
          (* Perform standard iteration through these use_ops. *)
          | use_op -> (lower_loc, [], use_op)
        in
        (* Loop through our parent use_op to get our property path. *)
        let lower_loc = loc_of_prop_compatibility_reason loc lower use_op in
        let (lower_loc, props, use_op) = loop lower_loc use_op in
        unwrap_frame_with_loc loc frames lower_loc use_op (FrameProperty (prop, props))
      | Frame (TupleElementCompatibility { n; lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op (FrameTupleIndex n)
      | Frame (TypeArgCompatibility { targ; lower; _ }, use_op) ->
        unwrap_frame loc frames lower use_op (FrameTypeArgument targ)
      | Frame (TypeParamBound { name }, use_op) ->
        unwrap_frame_without_loc
          loc
          frames
          use_op
          (FrameTypeParameterBound (Subst_name.string_of_subst_name name))
      | Frame (TypePredicateCompatibility, use_op) ->
        unwrap_frame_without_loc loc frames use_op FrameTypePredicate
      | Frame
          (InferredTypeForTypeGuardParameter { reason = param; is_return_false_statement }, use_op)
        ->
        unwrap_frame_without_loc
          loc
          frames
          use_op
          (FrameInferredTypeForTypeGuardParameter { param; is_return_false_statement })
      | Frame (FunCompatibility { lower; _ }, use_op) -> next_with_loc loc frames lower use_op
      | Frame (OpaqueTypeBound { opaque_t_reason = _ }, use_op) -> loop loc frames use_op
      | Frame (FunMissingArg _, use_op)
      | Frame (ImplicitTypeParam, use_op)
      | Frame (ReactConfigCheck, use_op)
      | Frame (ReactGetConfig _, use_op)
      | Frame (UnifyFlip, use_op)
      | Frame (CallFunCompatibility _, use_op)
      | Frame (TupleMapFunCompatibility _, use_op)
      | Frame (ObjMapFunCompatibility _, use_op)
      | Frame (ObjMapiFunCompatibility _, use_op)
      | Frame (MappedTypeKeyCompatibility _, use_op)
      | Frame (TupleAssignment _, use_op)
      | Frame (ReactDeepReadOnly (_, DROAnnot), use_op)
      | Frame (RendersCompatibility, use_op) ->
        loop loc frames use_op
    and next_with_loc loc frames frame_reason use_op =
      (* Skip this use_op, don't add a frame, but do use the loc to reposition
       * our primary location.
       *
       * If our current loc is inside our frame_loc then use our current loc
       * since it is the smallest possible loc in our frame_loc. *)
      let frame_loc = loc_of_aloc (loc_of_reason frame_reason) in
      let loc =
        if Loc.contains frame_loc loc then
          loc
        else
          frame_loc
      in
      loop loc frames use_op
    and unwrap_frame_with_loc loc frames frame_loc use_op frame =
      (* Add our frame message and reposition the location if appropriate.
       *
       * If our current loc is inside our frame_loc then use our current loc
       * since it is the smallest possible loc in our frame_loc. *)
      let frame_contains_loc = Loc.contains frame_loc loc in
      let loc =
        if frame_contains_loc then
          loc
        else
          frame_loc
      in
      (* Add our frame and recurse with the next use_op. *)
      let (all_frames, explanations) = frames in
      let frames = (frame :: all_frames, explanations) in
      loop loc frames use_op
    and unwrap_frame loc frames frame_reason use_op frame =
      let frame_loc = loc_of_aloc (loc_of_reason frame_reason) in
      unwrap_frame_with_loc loc frames frame_loc use_op frame
    and unwrap_frame_without_loc loc frames use_op frame =
      (* Same logic as `unwrap_frame` except we don't have a frame location. *)
      let (all_frames, explanations) = frames in
      let frames = (frame :: all_frames, explanations) in
      loop loc frames use_op
    and explanation loc frames use_op (explanation : 'loc explanation) =
      let (all_frames, explanations) = frames in
      let frames = (all_frames, explanation :: explanations) in
      loop loc frames use_op
    and unknown_root loc frames =
      (* We don't know what our root is! Return what we do know. *)
      let (all_frames, explanations) = frames in
      (None, loc, all_frames, explanations)
    and root_with_loc_and_specific_loc loc frames root_loc specific_loc root_message =
      (* Finish up be returning our root location, root message, primary loc,
       * and frames.

       * If our current loc is inside our root_loc then use our current loc
       * since it is the smallest possible loc in our root_loc. *)
      let loc =
        if Loc.contains root_loc loc && Loc.compare root_loc loc <> 0 then
          loc
        else
          specific_loc
      in
      (* Return our root loc and message in addition to the true primary loc
       * and frames. *)
      let (all_frames, explanations) = frames in
      (Some (root_loc, root_message), loc, all_frames, explanations)
    and root_with_specific_reason loc frames root_reason specific_reason root_message =
      let root_loc = loc_of_aloc (loc_of_reason root_reason) in
      let specific_loc = loc_of_aloc (loc_of_reason specific_reason) in
      root_with_loc_and_specific_loc loc frames root_loc specific_loc root_message
    and root loc frames root_reason root_message =
      let root_loc = loc_of_aloc (loc_of_reason root_reason) in
      root_with_loc_and_specific_loc loc frames root_loc root_loc root_message
    in
    (fun loc use_op -> loop loc ([], []) use_op)
  in
  (* Make a friendly error based on a use_op. The message we are provided should
   * not have any punctuation. Punctuation will be provided after the frames of
   * an error message. *)
  let mk_use_op_error loc use_op ?explanation message =
    let (root, loc, frames, explanations) = unwrap_use_ops (loc_of_aloc loc) use_op in
    let code = Flow_error.code_of_error error in
    let explanations =
      Base.Option.value_map ~f:(fun x -> x :: explanations) ~default:explanations explanation
    in
    mk_error ~trace_reasons ?root ~frames ~explanations loc code message
  in
  let mk_use_op_error_reason reason use_op ?explanation message =
    mk_use_op_error (loc_of_reason reason) use_op ?explanation message
  in
  let mk_no_frame_or_explanation_error reason message =
    let loc = loc_of_aloc (loc_of_reason reason) in
    let code = Flow_error.code_of_error error in
    mk_error ~trace_reasons ~frames:[] ~explanations:[] loc code message
  in

  (* Make a friendly error based on failed speculation. *)
  let mk_use_op_speculation_error loc use_op branches =
    match use_op with
    | Frame (MappedTypeKeyCompatibility { source_type; mapped_type }, use_op) ->
      mk_use_op_error
        (loc_of_reason source_type)
        use_op
        (MessageIncompatibleMappedTypeKey { source_type; mapped_type })
    | _ ->
      let (root, loc, frames, explanations) = unwrap_use_ops (loc_of_aloc loc) use_op in
      let error_code = Flow_error.code_of_error error in
      let speculation_errors =
        Base.List.map
          ~f:(fun (_, msg) ->
            let score = score_of_msg msg in
            let error =
              Flow_error.error_of_msg ~trace_reasons:[] ~source_file msg
              |> make_intermediate_error ~loc_of_aloc ~speculation:true
            in
            (score, error))
          branches
      in
      mk_speculation_error
        ~trace_reasons
        ~loc
        ~root
        ~frames
        ~explanations
        ~error_code
        speculation_errors
  in
  (* An error between two incompatible types. A "lower" type and an "upper"
   * type. The use_op describes the path which we followed to find
   * this incompatibility.
   *
   * This is a specialization of mk_incompatible_use_error. *)
  let mk_incompatible_error ?additional_explanation lower upper use_op =
    let ((lower, upper), use_op) = flip_contravariant (lower, upper) use_op in
    let make_error reason message =
      mk_use_op_error_reason reason use_op ?explanation:additional_explanation message
    in
    let lower = mod_lower_reason_according_to_use_ops lower use_op in
    match use_op with
    (* Add a custom message for Coercion root_use_ops that does not include the
     * upper bound. *)
    | Op (Coercion { from; _ }) -> make_error from (MessageShouldNotBeCoerced lower)
    | Frame
        ((TupleElementCompatibility { upper_optional; _ } | TupleAssignment { upper_optional }), _)
      when upper_optional && desc_of_reason lower = RVoid ->
      let upper = mk_reason (RTupleElement { name = None }) (loc_of_reason upper) in
      make_error lower (MessageCannotAssignToOptionalTupleElement { lower; upper })
    (* Ending with FunMissingArg gives us a different error message. Even though
     * this error was generated by an incompatibility, we want to show a more
     * descriptive error message. *)
    | Frame (FunMissingArg { def; op; _ }, use_op) ->
      let message =
        match use_op with
        | Op (FunCall _ | FunCallMethod _) ->
          let def =
            update_desc_reason
              (function
                | RFunctionType -> RFunction RNormal
                | desc -> desc)
              def
          in
          MessageFunctionRequiresAnotherArgument { def; from = None }
        | Frame (CallFunCompatibility { n }, _) -> MessageDollarCallArity { op; def; n }
        | Frame (TupleMapFunCompatibility { value }, _) ->
          MessageDollarTupleMapArity { op; value; def }
        | Frame (ObjMapFunCompatibility { value }, _) -> MessageDollarObjMapArity { op; value; def }
        | Frame (ObjMapiFunCompatibility { key; value }, _) ->
          MessageDollarObjMapiArity { op; key; value; def }
        | _ -> MessageFunctionRequiresAnotherArgument { def; from = Some op }
      in
      make_error op message
    | Frame (RendersCompatibility, _) -> make_error lower (MessageDoesNotRender { lower; upper })
    | _ ->
      let root_use_op = root_of_use_op use_op in
      (match root_use_op with
      (* Further customize functions with an implicit return. Functions with an
       * implicit return have a lower position which is not valuable. Also
       * clarify that the type was implicitly-returned.
       *
       * In flip_contravariant we flip upper/lower for all FunImplicitReturn. So
       * reverse those back as well. *)
      | FunImplicitReturn { upper = return; _ } ->
        make_error lower (MessageIncompatibleImplicitReturn { lower; upper; return })
      | ComponentRestParamCompatibility { rest_param } ->
        mk_no_frame_or_explanation_error
          rest_param
          (MessageIncompatibleComponentRestParam rest_param)
      (* Default incompatibility. *)
      | _ -> begin
        match (desc_of_reason lower, desc_of_reason upper) with
        | (RLongStringLit n, RStringLit _) ->
          make_error lower (MessageNonLiteralString { lower; upper; n })
        | _ -> make_error lower (MessageIncompatibleGeneral { lower; upper })
      end)
  in
  (* When we fail to find a property on an object we use this function to create
   * an error. prop_loc should be the position of the use which caused this
   * error. The use_op represents how we got to this error.
   *
   * If the use_op is a PropertyCompatibility frame then we encountered this
   * error while subtyping two objects. In this case we add a bit more
   * information to the error message. *)
  let mk_prop_missing_error prop_loc prop lower use_op suggestion =
    let (loc, lower, upper, use_op) =
      match use_op with
      (* If we are missing a property while performing property compatibility
       * then we are subtyping. Record the upper reason. *)
      | Frame (PropertyCompatibility { prop = compat_prop; lower; upper; _ }, use_op)
        when prop = Base.Option.map ~f:display_string_of_name compat_prop ->
        (loc_of_reason lower, lower, Some upper, use_op)
      (* Otherwise this is a general property missing error. *)
      | _ -> (prop_loc, lower, None, use_op)
    in
    let lower = mod_lower_reason_according_to_use_ops lower use_op in
    mk_use_op_error loc use_op (MessagePropMissing { lower; upper; prop; suggestion })
  in
  (* An error that occurs when some arbitrary "use" is incompatible with the
   * "lower" type. The use_op describes the path which we followed to find this
   * incompatibility.
   *
   * Similar to mk_incompatible_error except with any arbitrary *use*
   * instead of specifically an upper type. This error handles all use
   * incompatibilities in general. *)
  let mk_incompatible_use_error use_loc use_kind lower upper use_op =
    let lower = mod_lower_reason_according_to_use_ops lower use_op in
    let nope desc = mk_use_op_error use_loc use_op (MessageLowerIsNot { lower; desc }) in
    match use_kind with
    | IncompatibleElemTOfArrT -> nope "an array index"
    | IncompatibleGetPrivatePropT
    | IncompatibleSetPrivatePropT ->
      nope "a class with private properties"
    | IncompatibleMixedCallT -> mk_use_op_error use_loc use_op (MessageUnknownParameterTypes lower)
    | IncompatibleCallT -> nope "a function"
    | IncompatibleObjAssignFromTSpread
    | IncompatibleArrRestT ->
      nope "an array"
    | IncompatibleObjAssignFromT
    | IncompatibleObjRestT
    | IncompatibleGetKeysT
    | IncompatibleGetValuesT ->
      nope "an object"
    | IncompatibleMapTypeTObject ->
      mk_use_op_error use_loc use_op (MessageInvalidArgument { lower; upper })
    | IncompatibleMixinT
    | IncompatibleThisSpecializeT ->
      nope "a class"
    | IncompatibleSpecializeT
    | IncompatibleVarianceCheckT ->
      nope "a polymorphic type"
    | IncompatibleSuperT -> nope "inheritable"
    | IncompatibleUnaryArithT -> nope "a number"
    | IncompatibleGetPropT (prop_loc, prop)
    | IncompatibleSetPropT (prop_loc, prop)
    | IncompatibleHasOwnPropT (prop_loc, prop)
    | IncompatibleMethodT (prop_loc, prop) ->
      mk_prop_missing_error
        prop_loc
        (Base.Option.map ~f:display_string_of_name prop)
        lower
        use_op
        None
    | IncompatibleGetElemT prop_loc
    | IncompatibleSetElemT prop_loc
    | IncompatibleCallElemT prop_loc ->
      mk_prop_missing_error prop_loc None lower use_op None
    | IncompatibleGetStaticsT -> nope "an instance type"
    | IncompatibleBindT -> nope "a function type"
    (* unreachable or unclassified use-types. until we have a mechanical way
       to verify that all legit use types are listed above, we can't afford
       to throw on a use type, so mark the error instead *)
    | IncompatibleUnclassified ctor -> nope (spf "supported by unclassified use %s" ctor)
  in
  (* When an object property has a polarity that is incompatible with another
   * error then we create one of these errors. We use terms like "read-only" and
   * "write-only" to better reflect how the user thinks about these properties.
   * Other terminology could include "contravariant", "covariant", and
   * "invariant". Generally these terms are impenatrable to the average
   * JavaScript developer. If we had more documentation explaining these terms
   * it may be fair to use them in error messages. *)
  let mk_prop_polarity_mismatch_error prop (lower, lpole) (upper, upole) use_op =
    (* Remove redundant PropertyCompatibility if one exists. *)
    let use_op =
      match use_op with
      | Frame (PropertyCompatibility c, use_op)
        when Base.Option.map ~f:display_string_of_name c.prop = prop ->
        use_op
      | _ -> use_op
    in
    mk_use_op_error_reason
      lower
      use_op
      (MessagePropPolarityMismatch { lower; upper; lpole; upole; prop })
  in

  let intermediate_error =
    match (loc, friendly_message_of_msg loc_of_aloc msg) with
    | (Some loc, Error_message.Normal { features }) ->
      mk_error
        ~trace_reasons
        ~kind
        (loc_of_aloc loc)
        (Flow_error.code_of_error error)
        (MessageAlreadyFriendlyPrinted features)
    | (None, UseOp { loc; features; use_op; explanation }) ->
      let explanation = Base.Option.map explanation ~f:(fun e -> ExplanationAlreadyPrinted e) in
      mk_use_op_error loc use_op ?explanation (MessageAlreadyFriendlyPrinted features)
    | (None, PropMissing { loc; prop; reason_obj; use_op; suggestion }) ->
      mk_prop_missing_error loc prop reason_obj use_op suggestion
    | ( None,
        PropPolarityMismatch
          { prop; reason_lower; reason_upper; polarity_lower; polarity_upper; use_op }
      ) ->
      mk_prop_polarity_mismatch_error
        prop
        (reason_lower, polarity_lower)
        (reason_upper, polarity_upper)
        use_op
    | (None, IncompatibleUse { loc; upper_kind; reason_lower; reason_upper; use_op }) ->
      mk_incompatible_use_error loc upper_kind reason_lower reason_upper use_op
    | (None, Incompatible { reason_lower; reason_upper; use_op }) ->
      mk_incompatible_error reason_lower reason_upper use_op
    | ( None,
        IncompatibleEnum
          { reason_lower; reason_upper; use_op; enum_kind; representation_type; casting_syntax }
      ) ->
      let additional_explanation =
        match (enum_kind, representation_type) with
        | (ConcreteEnumKind, Some representation_type) ->
          Some (ExplanationConcreteEnumCasting { representation_type; casting_syntax })
        | (AbstractEnumKind, _) -> Some ExplanationAbstractEnumCasting
        | _ -> None
      in
      mk_incompatible_error ?additional_explanation reason_lower reason_upper use_op
    | (None, Error_message.Speculation { loc; use_op; branches }) ->
      mk_use_op_speculation_error loc use_op branches
    | (None, Error_message.Normal _)
    | (Some _, _) ->
      raise (ImproperlyFormattedError (map_loc_of_error_message loc_of_aloc msg))
  in
  let misplaced_source_file =
    match Loc.source intermediate_error.loc with
    | Some file
      when (not speculation) && (not (File_key.is_lib_file source_file)) && file <> source_file ->
      Some source_file
    | _ -> None
  in
  { intermediate_error with misplaced_source_file }

let make_intermediate_error = make_intermediate_error ~speculation:false

let to_printable_error :
      'loc.
      loc_of_aloc:('loc -> Loc.t) ->
      strip_root:File_path.t option ->
      'loc intermediate_error ->
      Loc.t Flow_errors_utils.printable_error =
 fun ~loc_of_aloc ~strip_root intermediate_error ->
  let open Flow_errors_utils in
  let text = Friendly.text in
  let code = Friendly.code in
  let ref = Friendly.ref_map loc_of_aloc in
  let desc = Friendly.desc_of_reason_desc in
  let explanation_to_friendly_msgs = function
    | ExplanationAlreadyPrinted features -> features
    | ExplanationAbstractEnumCasting ->
      [
        text "You can explicitly cast your enum value to its representation type using ";
        code "<expr>.valueOf()";
      ]
    | ExplanationArrayInvariantTyping ->
      [
        text "Arrays are invariantly typed. See ";
        text
          "https://flow.org/en/docs/faq/#why-cant-i-pass-an-arraystring-to-a-function-that-takes-an-arraystring-number";
      ]
    | ExplanationMultiplatform ->
      [
        text "Read the docs on Flow's multi-platform support for more information: ";
        text "https://flow.org/en/docs/react/multiplatform";
      ]
    | ExplanationPropertyInvariantTyping ->
      [
        text "This property is invariantly typed. See ";
        text
          "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number";
      ]
    | ExplanationReactComponentRefRequirement ->
      [text "The "; code "ref"; text " parameter must be a subtype of "; code "React.RefSetter"]
    | ExplanationRenderTypeRequirement ->
      [
        text "Render types must be a subtype of ";
        code "React.Node";
        text " or a reference to a component-syntax component";
      ]
    | ExplanationTypeGuardCompatibility ->
      [text "A user defined type guard needs to be compatible with its parameter's type"]
    | ExplanationConstrainedAssign { name; declaration; providers; array } ->
      let noun =
        if array then
          "element"
        else
          "assignment"
      in
      let assignments =
        match providers with
        | [] -> (* should not happen *) [text (spf "one of its initial %ss" noun)]
        | [r] when Loc.equal (loc_of_aloc r) (loc_of_aloc declaration) ->
          [
            text "its ";
            ref
              (mk_reason
                 (RCustom
                    ( if array then
                      "initial element"
                    else
                      "initializer"
                    )
                 )
                 declaration
              );
          ]
        | [r] -> [text "its "; ref (mk_reason (RCustom ("initial " ^ noun)) r)]
        | providers ->
          text (spf "one of its initial %ss" noun)
          :: (Base.List.map ~f:(Friendly.no_desc_ref_map loc_of_aloc) providers
             |> Base.List.intersperse ~sep:(text ",")
             )
      in
      [text "All writes to "; code name; text " must be compatible with the type of "]
      @ assignments
      @ [
          text ". Add an annotation to ";
          ref (mk_reason (RIdentifier (OrdinaryName name)) declaration);
          text " if a different type is desired";
        ]
    | ExplanationConcreteEnumCasting { representation_type; casting_syntax } ->
      let example =
        let open Options.CastingSyntax in
        match casting_syntax with
        | Colon -> spf "(<expr>: %s)" representation_type
        | Both
        | As ->
          spf "<expr> as %s" representation_type
      in
      [
        text "You can explicitly cast your enum value to a ";
        text representation_type;
        text " using ";
        code example;
      ]
    | ExplanationReactComponentPropsDeepReadOnly props_loc ->
      [
        text "React ";
        ref (mk_reason (RCustom "component properties") props_loc);
        text " and their nested props and elements cannot be written to";
      ]
    | ExplanationReactHookArgsDeepReadOnly props_loc ->
      [
        text "React ";
        ref (mk_reason (RCustom "hook arguments") props_loc);
        text " and their nested elements cannot be written to";
      ]
    | ExplanationReactHookReturnDeepReadOnly hook_loc ->
      [
        text "The return value of a ";
        ref (mk_reason (RCustom "React hook") hook_loc);
        text " cannot be written to";
      ]
  in
  let frame_to_friendly_msgs = function
    | FrameAnonymous -> []
    | FrameArrayElement -> [text "array element"]
    | FrameCallableSignature -> [text "the callable signature"]
    | FrameEnumRepresentationType -> [text "the enum's representation type"]
    | FrameFunNthArgument n -> [text "the "; text (Utils_js.ordinal n); text " argument"]
    | FrameFunThisArgument -> [text "the "; code "this"; text " argument"]
    | FrameFunNthParam n -> [text "the "; text (Utils_js.ordinal n); text " parameter"]
    | FrameFunThisParam -> [text "the "; code "this"; text " parameter"]
    | FrameIndexerProperty -> [text "the indexer property"]
    | FrameIndexerPropertyKey -> [text "the indexer property's key"]
    | FrameInferredTypeForTypeGuardParameter { param; is_return_false_statement } ->
      [text "the type inferred for "; ref param]
      @
      if is_return_false_statement then
        [
          text ". Consider replacing the body of this predicate ";
          text "function with a single conditional expression";
        ]
      else
        []
    | FrameProperty (prop, props) ->
      [
        text "property ";
        code
          (List.fold_left
             (fun acc prop -> display_string_of_name prop ^ "." ^ acc)
             (display_string_of_name prop)
             props
          );
      ]
    | FrameTupleIndex n -> [text "index "; text (string_of_int n)]
    | FrameTypeArgument targ -> [text "type argument "; ref targ]
    | FrameTypeParameterBound name -> [text "type argument "; code name]
    | FrameTypePredicate -> [text "the type predicate"]
    | FrameReturnValue -> [text "the return value"]
  in
  let root_msg_to_friendly_msgs = function
    | RootCannotAccessIndex { index; object_ } ->
      [text "Cannot access "; desc index; text " on "; desc object_]
    | RootCannotAdd { left; right } -> [text "Cannot add "; desc left; text " and "; desc right]
    | RootCannotAssign { init; target = None } ->
      [text "Cannot assign "; desc init; text " to variable"]
    | RootCannotAssign { init; target = Some target } ->
      [text "Cannot assign "; desc init; text " to "; desc target]
    | RootCannotCall fn -> [text "Cannot call "; desc fn]
    | RootCannotCallWithNamedParam { fn; lower; name } ->
      [text "Cannot call "; desc fn; text " with "; desc lower; text " bound to "; code name]
    | RootCannotCallWithNthParam { fn; lower; n : int } ->
      [
        text "Cannot call ";
        desc fn;
        text " with ";
        desc lower;
        text " bound to ";
        text (spf "the %s parameter" (Utils_js.ordinal n));
      ]
    | RootCannotCallObjectAssign op -> [text "Incorrect arguments passed to "; desc op]
    | RootCannotCast { lower; upper } -> [text "Cannot cast "; desc lower; text " to "; desc upper]
    | RootCannotCheckAgainst { test; discriminant } ->
      [text "Invalid check of "; desc test; text " against "; ref discriminant]
    | RootCannotCoerce { from; target } ->
      [text "Cannot coerce "; desc from; text " to "; desc target]
    | RootCannotConformToCommonInterface -> [text "Cannot conform to common interface module"]
    | RootCannotCompareWithProperty { sentinel; obj; key } ->
      [text "Cannot compare "; ref sentinel; text " with property "; code key; text " of "; ref obj]
    | RootCannotCreateElement component -> [text "Cannot create "; desc component; text " element"]
    | RootCannotDeclareRef -> [text "Cannot declare ref"]
    | RootCannotDeclarePredicate { predicate_loc; fn } ->
      [
        text "Cannot declare a ";
        ref (mk_reason (RCustom "type predicate") predicate_loc);
        text " for ";
        ref fn;
      ]
    | RootCannotDefineClassMethod { method_; name } ->
      [text "Cannot define "; ref method_; text " on "; desc name]
    | RootCannotDefineShadowedProtoProperty -> [text "Cannot define shadowed proto property"]
    | RootCannotDelete prop -> [text "Cannot delete "; desc prop]
    | RootCannotExpectImplicitReturn { upper; fn } ->
      [text "Cannot expect "; desc upper; text " as the return type of "; desc fn]
    | RootCannotExtendClass { extends; def } ->
      [text "Cannot extend "; ref extends; text " with "; desc def]
    | RootCannotGetProp prop -> [text "Cannot get "; desc prop]
    | RootCannotGetRest op -> [text "Cannot get rest of "; desc op]
    | RootCannotImplementClass { implements; def } ->
      [text "Cannot implement "; ref implements; text " with "; desc def]
    | RootCannotInitializeField { field; body } ->
      [text "Cannot initialize "; desc field; text " with "; desc body]
    | RootCannotInstantiateEval type_ -> [text "Cannot instantiate "; ref type_]
    | RootCannotInstantiateTypeApp type_ -> [text "Cannot instantiate "; desc type_]
    | RootCannotInstantiateRenderType type_ ->
      [text "Cannot use "; ref type_; text " as a render type"]
    | RootCannotReturn value -> [text "Cannot return "; desc value]
    | RootCannotShadowProto proto -> [text "Cannot shadow proto "; ref proto]
    | RootCannotShadowProtoProperty -> [text "Cannot shadow proto property"]
    | RootCannotSpread op -> [text "Cannot spread "; desc op]
    | RootCannotUpdate prop -> [text "Cannot update "; desc prop]
    | RootCannotUseInferTypeBound { bound; infer } ->
      [text "Cannot use "; desc bound; text " as the bound of infer type "; desc infer]
    | RootCannotUseTypeGuard { guard_type; param_name } ->
      [
        text "Cannot use ";
        ref guard_type;
        text (spf " as type prediate for parameter `%s`" param_name);
      ]
    | RootCannotYield value -> [text "Cannot yield "; desc value]
  in
  let msg_to_friendly_msgs = function
    | MessageAlreadyFriendlyPrinted features -> features
    | MessageCannotAssignToOptionalTupleElement { lower; upper } ->
      [
        text "you cannot assign ";
        ref lower;
        text " to optional ";
        ref upper;
        text " (to do so, add ";
        code "| void";
        text " to the tuple element type)";
      ]
    | MessageDoesNotRender { lower; upper } ->
      (* We replace the desc of the reason so we can say "LHS" does not render "RHS" instead of
         * "LHS" does not render "renders RHS" *)
      let rec loop = function
        | RRenderType desc -> loop desc
        | RRenderMaybeType desc -> loop desc
        | RRenderStarType desc -> loop desc
        | desc -> desc
      in
      let lower_desc = loop (desc_of_reason lower) in
      let lower_r = replace_desc_reason lower_desc lower in
      let upper_desc = loop (desc_of_reason upper) in
      let upper_r = replace_desc_reason upper_desc upper in
      [ref lower_r; text " does not render "; ref upper_r]
    | MessageDollarCallArity { op; def; n } ->
      let exp =
        if n = 1 then
          "one argument"
        else
          string_of_int n ^ " arguments"
      in
      [
        ref op;
        text (spf " passes only %s to the provided function type, but " exp);
        ref def;
        text (spf " expects more than %s. See " exp);
        text Friendly.(docs.call);
        text " for documentation";
      ]
    | MessageDollarObjMapArity { op; value; def } ->
      [
        ref op;
        text " expects the provided function type to take only one argument, the value type ";
        ref value;
        text ", but ";
        ref def;
        text " takes more than one argument. See ";
        text Friendly.(docs.objmap);
        text " for documentation";
      ]
    | MessageDollarObjMapiArity { op; key; value; def } ->
      [
        ref op;
        text " expects the provided function type to take only two arguments, the key type ";
        ref key;
        text " and the value type ";
        ref value;
        text ", but ";
        ref def;
        text " takes more than two arguments. See ";
        text Friendly.(docs.objmapi);
        text " for documentation";
      ]
    | MessageDollarTupleMapArity { op; value; def } ->
      [
        ref op;
        text " expects the provided function type to take only one argument, the value type ";
        ref value;
        text ", but ";
        ref def;
        text " takes more than one argument. See ";
        text Friendly.(docs.tuplemap);
        text " for documentation";
      ]
    | MessageFunctionRequiresAnotherArgument { def; from = None } ->
      [ref def; text " requires another argument"]
    | MessageFunctionRequiresAnotherArgument { def; from = Some from } ->
      [ref def; text " requires another argument from "; ref from]
    | MessageIncompatibleImplicitReturn { lower; upper; return } ->
      let upper_loc = loc_of_aloc (loc_of_reason upper) in
      let return_loc = loc_of_aloc (loc_of_reason return) in
      [ref lower; text " is incompatible with "]
      @
      if Loc.compare return_loc upper_loc = 0 then
        [text "implicitly-returned "; desc (desc_of_reason upper)]
      else
        [ref upper]
    | MessageIncompatibleComponentRestParam rest_param ->
      [
        text "Cannot use ";
        ref rest_param;
        text
          " as a component rest param. Component rest params must use an object type and cannot be optional";
      ]
    | MessageIncompatibleGeneral { lower; upper } ->
      [ref lower; text " is incompatible with "; ref upper]
    | MessageIncompatibleMappedTypeKey { source_type; mapped_type } ->
      [
        ref source_type;
        text " is incompatible with ";
        code "string | number | symbol";
        text ", so it cannot be used to generate keys for ";
        ref mapped_type;
      ]
    | MessageInvalidArgument { lower; upper } ->
      [ref lower; text " is not a valid argument of "; ref upper]
    | MessageLowerIsNot { lower; desc } -> [ref lower; text " is not "; text desc]
    | MessageNonLiteralString { lower; upper; n } ->
      [
        ref lower;
        text " is incompatible with ";
        ref upper;
        text " because strings longer than ";
        code (string_of_int n);
        text " characters are not treated as literals";
      ]
    | MessagePropMissing { lower; upper; prop; suggestion } ->
      (* If we were subtyping that add to the error message so our user knows what
       * object required the missing property. *)
      let prop_message = mk_prop_message prop in
      let suggestion =
        match suggestion with
        | Some suggestion -> [text " (did you mean "; code suggestion; text "?)"]
        | None -> []
      in
      (match upper with
      | Some upper ->
        prop_message
        @ suggestion
        @ [text " is missing in "; ref lower; text " but exists in "]
        @ [ref upper]
      | None ->
        (match prop with
        | None when is_nullish_reason lower -> [ref lower; text " does not have properties"]
        | _ -> prop_message @ suggestion @ [text " is missing in "; ref lower]))
    | MessagePropPolarityMismatch { lower; upper; lpole; upole; prop } ->
      let expected = polarity_explanation (lpole, upole) in
      let actual = polarity_explanation (upole, lpole) in
      mk_prop_message prop
      @ [
          text " is ";
          text expected;
          text " in ";
          ref lower;
          text " but ";
          text actual;
          text " in ";
          ref upper;
        ]
    | MessageShouldNotBeCoerced lower -> [ref lower; text " should not be coerced"]
    | MessageUnknownParameterTypes lower ->
      [text "the parameter types of an "; ref lower; text " are unknown"]
  in
  let mk_info reason extras =
    let desc = string_of_desc (desc_of_reason reason) in
    (* For descriptions that are an identifier wrapped in primes, e.g. `A`, then
     * we want to unwrap the primes and just show A. This looks better in infos.
     * However, when an identifier wrapped with primes is inside some other text
     * then we want to keep the primes since they help with readability. *)
    let desc =
      if
        String.length desc > 2
        && desc.[0] = '`'
        && desc.[String.length desc - 1] = '`'
        && not (String.contains desc ' ')
      then
        String.sub desc 1 (String.length desc - 2)
      else
        desc
    in
    (loc_of_aloc (loc_of_reason reason), desc :: extras)
  in
  let info_of_reason r = mk_info r [] in
  let rec convert_error_message
      { kind; trace_reasons; loc; error_code; root; message; misplaced_source_file = _ } =
    let trace_infos = Base.Option.map ~f:(Base.List.map ~f:info_of_reason) trace_reasons in
    let root = Base.Option.map root ~f:(fun (loc, msg) -> (loc, root_msg_to_friendly_msgs msg)) in
    match message with
    | SingletonMessage { message; frames; explanations } ->
      let frames = Option.map (List.map frame_to_friendly_msgs) frames in
      let explanations = Option.map (List.map explanation_to_friendly_msgs) explanations in
      mk_error
        ~kind
        ?trace_infos
        ?root
        ?frames
        ?explanations
        loc
        error_code
        (msg_to_friendly_msgs message)
    | SpeculationMessage { frames; explanations; branches } ->
      mk_speculation_error
        ~kind
        ?trace_infos
        ~loc
        ~root
        ~frames:(List.map frame_to_friendly_msgs frames)
        ~explanations:(List.map explanation_to_friendly_msgs explanations)
        ~error_code
        (Base.List.map branches ~f:(fun (i, e) -> (i, convert_error_message e)))
  in
  let printable_error = convert_error_message intermediate_error in
  match intermediate_error.misplaced_source_file with
  | None -> printable_error
  | Some source_file -> patch_misplaced_error ~strip_root source_file printable_error

let make_errors_printable ~loc_of_aloc ~strip_root errors =
  let f err acc =
    let err =
      err |> make_intermediate_error ~loc_of_aloc |> to_printable_error ~loc_of_aloc ~strip_root
    in
    Flow_errors_utils.ConcreteLocPrintableErrorSet.add err acc
  in
  Flow_error.ErrorSet.fold f errors Flow_errors_utils.ConcreteLocPrintableErrorSet.empty
