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
let rec score_of_use_op use_op =
  fold_use_op
    (function
      | Speculation use_op -> score_of_use_op use_op
      | _ -> 0)
    (fun acc frame ->
      acc
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
      (* This is just a marker frame for custom errors. *)
      | OpaqueTypeCustomErrorCompatibility _ -> 0
      | _ -> frame_score)
    use_op

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
  let score = util_use_op_of_msg 0 score_of_use_op msg in
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
    | EPropNotFoundInLookup { use_op = Frame (PropertyCompatibility _, _); _ } ->
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
  | OpaqueTypeLowerBoundCompatibility c ->
    OpaqueTypeLowerBoundCompatibility { lower = c.upper; upper = c.lower }
  | OpaqueTypeUpperBoundCompatibility c ->
    OpaqueTypeUpperBoundCompatibility { lower = c.upper; upper = c.lower }
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
  | ( TupleAssignment _ | TypeParamBound _ | OpaqueTypeLowerBound _ | OpaqueTypeUpperBound _
    | FunMissingArg _ | ImplicitTypeParam | ReactGetConfig _ | OpaqueTypeCustomErrorCompatibility _
    | UnifyFlip | ConstrainedAssignment _ | MappedTypeKeyCompatibility _ | TypeGuardCompatibility
    | RendersCompatibility | ReactDeepReadOnly _ ) as use_op ->
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
        error_of_msg ~source_file:(source_file error) new_msg
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
    | EIncompatibleWithUseOp { reason_lower; reason_upper; use_op; explanation } ->
      let ((reason_lower', reason_upper), use_op) =
        dedupe_by_flip (reason_lower, reason_upper) use_op
      in
      reason_lower = reason_lower'
      || is_not_duplicate
           (EIncompatibleWithUseOp
              { reason_lower = reason_lower'; reason_upper; use_op; explanation }
           )
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
    | EPropNotFoundInLookup { prop_name; reason_obj; reason_prop; use_op; suggestion } ->
      (* PropNotFound error will always display the missing vs existing prop in the same order. *)
      let use_op' = remove_unify_flip use_op in
      use_op = use_op'
      || is_not_duplicate
           (EPropNotFoundInLookup
              { prop_name; reason_obj; reason_prop; use_op = use_op'; suggestion }
           )
    | _ -> true
  in
  Flow_error.ErrorSet.filter retain_error original_errors

open Flow_intermediate_error_types

let mk_error
    ?(kind = Flow_errors_utils.InferError) ?root ?frames ?explanations loc error_code message =
  {
    kind;
    loc;
    root;
    error_code;
    message = SingletonMessage { message; frames; explanations };
    misplaced_source_file = None;
    unsuppressable = false;
  }

let mk_speculation_error
    ?(kind = Flow_errors_utils.InferError)
    ~loc
    ~root
    ~frames
    ~explanations
    ~error_code
    speculation_errors =
  (* Flatten the speculation branches to remove nested speculation errors with no root *)
  let branches =
    let rec flatten_speculation_branches acc_frames acc_explanations acc = function
      | [] -> acc
      | (score, error) :: branches ->
        (match error.message with
        (* If we have a speculation error with no frames and no root then we want
         * to flatten the branches of that error. *)
        | SpeculationMessage
            { branches = nested_branches; frames = inner_frames; explanations = inner_explanations }
          when Base.Option.is_none error.root ->
          (* We don't perform tail-call recursion here, but it's unlikely that
           * speculations will be so deeply nested that we blow the stack. *)
          let acc =
            flatten_speculation_branches
              (inner_frames :: acc_frames)
              (inner_explanations :: acc_explanations)
              acc
              nested_branches
          in
          flatten_speculation_branches acc_frames acc_explanations acc branches
        | SpeculationMessage
            { branches = nested_branches; frames = inner_frames; explanations = inner_explanations }
          ->
          let frames = Base.List.concat (List.rev (inner_frames :: acc_frames)) in
          let explanations = Base.List.concat (List.rev (inner_explanations :: acc_explanations)) in
          let message = SpeculationMessage { branches = nested_branches; frames; explanations } in
          let acc = (score, { error with message }) :: acc in
          flatten_speculation_branches acc_frames acc_explanations acc branches
        | SingletonMessage { message; frames = inner_frames; explanations = inner_explanations } ->
          let frames =
            match (inner_frames, acc_frames) with
            | (Some inner_frames, _) ->
              Some (Base.List.concat (List.rev (inner_frames :: acc_frames)))
            | (None, _ :: _) -> Some (Base.List.concat (List.rev acc_frames))
            | (None, []) -> None
          in
          let explanations =
            match (inner_explanations, acc_explanations) with
            | (Some inner_explanations, _) ->
              Some (Base.List.concat (List.rev (inner_explanations :: acc_explanations)))
            | (None, _ :: _) -> Some (Base.List.concat (List.rev acc_explanations))
            | (None, []) -> None
          in
          let message = SingletonMessage { message; frames; explanations } in
          let acc = (score, { error with message }) :: acc in
          flatten_speculation_branches acc_frames acc_explanations acc branches)
    in
    flatten_speculation_branches [] [] [] speculation_errors
    |> Base.List.rev
    |> Base.List.stable_sort ~compare:(fun (s1, _) (s2, _) -> Int.compare s1 s2)
  in
  {
    kind;
    loc;
    root;
    error_code;
    message = SpeculationMessage { frames; explanations; branches };
    misplaced_source_file = None;
    unsuppressable = false;
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
    | Frame (OpaqueTypeUpperBound { opaque_t_reason }, use_op) ->
      mod_lower_reason_according_to_use_ops opaque_t_reason use_op
    | Frame (_, use_op) -> mod_lower_reason_according_to_use_ops lower use_op
    | Op _ -> lower
  in
  let rec mod_upper_reason_according_to_use_ops upper = function
    | Frame (OpaqueTypeLowerBound { opaque_t_reason }, use_op) ->
      mod_upper_reason_according_to_use_ops opaque_t_reason use_op
    | Frame (_, use_op) -> mod_upper_reason_according_to_use_ops upper use_op
    | Op _ -> upper
  in
  let desc = Reason.desc_of_reason ~unwrap:false in
  (* Unwrap a use_op for the friendly error format. Takes the smallest location
   * where we found the error and a use_op which we will unwrap. *)
  let unwrap_use_ops :
      Loc.t ->
      'loc virtual_use_op ->
      (Loc.t * 'loc root_message) option
      * 'loc Flow_intermediate_error_types.message option
      * Loc.t
      * 'loc frame list
      * 'loc explanation list =
    (* We will optionally include lower and upper pair to provide a TS-like stacked errors.
     * We only do this if the upper bound has a nice name. *)
    let opt_incompatibility_pair use_op ((_lower, upper) as pair) =
      let is_named_type = function
        | REnum { name = Some _ }
        | RType _
        | RTypeAlias (_, Some _, _) ->
          true
        | _ -> false
      in
      if is_named_type (desc_of_reason upper) then
        match use_op with
        | Op (AssignVar { var = Some _; _ })
        | Op (Cast _)
        | Op (ClassExtendsCheck _)
        | Op (ClassImplementsCheck _) ->
          (* For these roots, the upper bound is already a named type, so there is no point of
           * repeating the incompatibility in the outer most frame. It helps to prevent error
           * messages like `Cannot extends A with B because B is incompatible with A in ...` *)
          None
        | _ -> Some pair
      else
        None
    in
    let loc_of_prop_compatibility_reason loc reason = function
      (* If we are checking class extensions or implementations then the
       * object reason will point to the class name. So don't reposition with
       * this reason. *)
      | Op (ClassExtendsCheck _) -> loc
      | Op (ClassImplementsCheck _) -> loc
      | _ -> loc_of_aloc (loc_of_reason reason)
    in
    let rec loop_to_form_access_chain lower_loc = function
      (* Don't match $call properties since they have special
       * meaning. As defined above. *)
      | Frame (PropertyCompatibility { prop = Some prop; lower = lower'; upper = upper'; _ }, use_op)
      (* TODO the $-prefixed names should be internal *)
        when prop <> OrdinaryName "$call"
             && Base.Option.is_none (opt_incompatibility_pair use_op (lower', upper')) ->
        let lower_loc' = loc_of_prop_compatibility_reason lower_loc lower' use_op in
        (* Perform the same frame location unwrapping as we do in our
         * general code. *)
        let lower_loc =
          if Loc.contains lower_loc' lower_loc then
            lower_loc
          else
            lower_loc'
        in
        let (lower_loc, props, use_op) = loop_to_form_access_chain lower_loc use_op in
        (lower_loc, PropSegment prop :: props, use_op)
      | Frame (TupleElementCompatibility { n; lower = lower'; upper = upper'; _ }, use_op)
        when Base.Option.is_none (opt_incompatibility_pair use_op (lower', upper')) ->
        let lower_loc' = loc_of_aloc (loc_of_reason lower') in
        let lower_loc =
          if Loc.contains lower_loc' lower_loc then
            lower_loc
          else
            lower_loc'
        in
        let (lower_loc, props, use_op) = loop_to_form_access_chain lower_loc use_op in
        (lower_loc, TupleIndexSegment n :: props, use_op)
      (* Perform standard iteration through these use_ops. *)
      | use_op -> (lower_loc, [], use_op)
    in
    let rec loop ~loc ~frames ~use_op ~custom_error_message =
      match use_op with
      | Op UnknownUse -> unknown_root ~loc ~frames ~custom_error_message
      | Op (Type.Speculation _) when speculation -> unknown_root ~loc ~frames ~custom_error_message
      | Op (Type.Speculation use) -> loop ~loc ~frames ~use_op:use ~custom_error_message
      | Op (ObjectAddComputedProperty { op }) ->
        root
          ~loc
          ~frames
          ~root_reason:op
          ~root_message:RootCannotAddComputedProperty
          ~custom_error_message
      | Op (ObjectSpread { op }) ->
        root
          ~loc
          ~frames
          ~root_reason:op
          ~root_message:(RootCannotSpread (desc op))
          ~custom_error_message
      | Op (ObjectRest { op }) ->
        root
          ~loc
          ~frames
          ~root_reason:op
          ~root_message:(RootCannotGetRest (desc op))
          ~custom_error_message
      | Op (ObjectChain { op }) ->
        root
          ~loc
          ~frames
          ~root_reason:op
          ~root_message:(RootCannotCallObjectAssign (desc op))
          ~custom_error_message
      | Op (AssignVar { var; init }) ->
        root
          ~loc
          ~frames
          ~root_reason:init
          ~root_message:(RootCannotAssign { init = desc init; target = Option.map desc var })
          ~custom_error_message
      | Op (DeleteVar { var }) ->
        root
          ~loc
          ~frames
          ~root_reason:var
          ~root_message:(RootCannotDelete (desc var))
          ~custom_error_message
      | Op (InitField { op; body }) ->
        root
          ~loc
          ~frames
          ~root_reason:op
          ~root_message:(RootCannotInitializeField { field = desc op; body = desc body })
          ~custom_error_message
      | Op (Cast { lower; upper }) ->
        root
          ~loc
          ~frames
          ~root_reason:lower
          ~root_message:(RootCannotCast { lower = desc lower; upper = desc upper })
          ~custom_error_message
      | Op (ClassExtendsCheck { extends; def }) ->
        root
          ~loc
          ~frames
          ~root_reason:def
          ~root_message:(RootCannotExtendClass { extends; def = desc def })
          ~custom_error_message
      | Op (ClassMethodDefinition { name; def }) ->
        root
          ~loc
          ~frames
          ~root_reason:def
          ~root_message:(RootCannotDefineClassMethod { method_ = def; name = desc name })
          ~custom_error_message
      | Op (ClassImplementsCheck { implements; def; _ }) ->
        root
          ~loc
          ~frames
          ~root_reason:def
          ~root_message:(RootCannotImplementClass { implements; def = desc def })
          ~custom_error_message
      | Op (ClassOwnProtoCheck { prop; own_loc; proto_loc }) ->
        (match (own_loc, proto_loc) with
        | (None, None) -> unknown_root ~loc ~frames ~custom_error_message
        | (Some loc, None) ->
          let def = mk_reason (RProperty (Some prop)) loc in
          root
            ~loc:(loc_of_aloc loc)
            ~frames
            ~root_reason:def
            ~root_message:RootCannotShadowProtoProperty
            ~custom_error_message
        | (None, Some loc) ->
          let def = mk_reason (RProperty (Some prop)) loc in
          root
            ~loc:(loc_of_aloc loc)
            ~frames
            ~root_reason:def
            ~root_message:RootCannotDefineShadowedProtoProperty
            ~custom_error_message
        | (Some own_loc, Some proto_loc) ->
          let def = mk_reason (RProperty (Some prop)) own_loc in
          let proto = mk_reason (RProperty (Some prop)) proto_loc in
          root
            ~loc
            ~frames
            ~root_reason:def
            ~root_message:(RootCannotShadowProto proto)
            ~custom_error_message)
      | Op (Coercion { from; target }) ->
        root
          ~loc
          ~frames
          ~root_reason:from
          ~root_message:(RootCannotCoerce { from = desc from; target = desc target })
          ~custom_error_message
      | Op (ConformToCommonInterface { self_sig_loc; self_module_loc; originate_from_import }) ->
        let frames =
          let (all_frames, explanations) = frames in
          (all_frames, ExplanationMultiplatform :: explanations)
        in
        root_with_loc_and_specific_loc
          ~loc
          ~frames
          ~root_loc:(loc_of_aloc self_module_loc)
          ~specific_loc:(loc_of_aloc self_sig_loc)
          ~root_message:(RootCannotConformToCommonInterface { originate_from_import })
          ~custom_error_message
      | Op (DeclareComponentRef { op }) ->
        root ~loc ~frames ~root_reason:op ~root_message:RootCannotDeclareRef ~custom_error_message
      | Op (FunCall { op; fn; _ }) ->
        root_with_specific_reason
          ~loc
          ~frames
          ~root_reason:op
          ~specific_reason:fn
          ~root_message:(RootCannotCall (desc fn))
          ~custom_error_message
      | Op (FunCallMethod { op; fn; prop; _ }) ->
        root_with_specific_reason
          ~loc
          ~frames
          ~root_reason:op
          ~specific_reason:prop
          ~root_message:(RootCannotCall (desc fn))
          ~custom_error_message
      | Op (RenderTypeInstantiation { render_type }) ->
        root
          ~loc
          ~frames
          ~root_reason:render_type
          ~root_message:RootCannotInstantiateRenderType
          ~custom_error_message
      | Frame
          ( FunParam _,
            (Op (Type.Speculation (Op (FunCall _ | FunCallMethod _ | JSXCreateElement _))) as use_op)
          ) ->
        loop ~loc ~frames ~use_op ~custom_error_message
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
        root ~loc ~frames ~root_reason:lower ~root_message:root_msg ~custom_error_message
      | Op (FunReturnStatement { value }) ->
        root
          ~loc
          ~frames
          ~root_reason:value
          ~root_message:(RootCannotReturn (desc value))
          ~custom_error_message
      | Op (FunImplicitReturn { upper; fn; type_guard = true }) ->
        root
          ~loc
          ~frames
          ~root_reason:upper
          ~root_message:(RootCannotDeclareTypeGuard { type_guard_loc = loc_of_reason upper; fn })
          ~custom_error_message
      | Op (FunImplicitReturn { upper; fn; _ }) ->
        root
          ~loc
          ~frames
          ~root_reason:upper
          ~root_message:(RootCannotExpectImplicitReturn { upper = desc upper; fn = desc fn })
          ~custom_error_message
      | Op (GeneratorYield { value }) ->
        root
          ~loc
          ~frames
          ~root_reason:value
          ~root_message:(RootCannotYield (desc value))
          ~custom_error_message
      | Op (GetExport prop) ->
        root
          ~loc
          ~frames
          ~root_reason:prop
          ~root_message:(RootCannotGetProp (desc prop))
          ~custom_error_message
      | Op (GetProperty prop) ->
        root
          ~loc
          ~frames
          ~root_reason:prop
          ~root_message:(RootCannotGetProp (desc prop))
          ~custom_error_message
      | Op (IndexedTypeAccess { _object; index }) ->
        root
          ~loc
          ~frames
          ~root_reason:index
          ~root_message:(RootCannotAccessIndex { index = desc index; object_ = desc _object })
          ~custom_error_message
      | Op (InferBoundCompatibilityCheck { bound; infer }) ->
        root
          ~loc
          ~frames
          ~root_reason:bound
          ~root_message:(RootCannotUseInferTypeBound { infer = desc infer })
          ~custom_error_message
      | Frame (FunParam _, Op (JSXCreateElement { op; component; _ }))
      | Op (JSXCreateElement { op; component; _ })
      | Op (ReactCreateElementCall { op; component; _ }) ->
        root_with_specific_reason
          ~loc
          ~frames
          ~root_reason:op
          ~specific_reason:component
          ~root_message:(RootCannotCreateElement (desc component))
          ~custom_error_message
      | Op (ReactGetIntrinsic { literal }) ->
        root
          ~loc
          ~frames
          ~root_reason:literal
          ~root_message:(RootCannotCreateElement (desc literal))
          ~custom_error_message
      | Frame (FunParam _, Op (RecordCreate { op; constructor; _ }))
      | Op (RecordCreate { op; constructor; _ }) ->
        root_with_specific_reason
          ~loc
          ~frames
          ~root_reason:op
          ~specific_reason:constructor
          ~root_message:(RootCannotCreateRecord (desc constructor))
          ~custom_error_message
      | Op (TypeApplication { type_ }) ->
        root
          ~loc
          ~frames
          ~root_reason:type_
          ~root_message:(RootCannotInstantiateTypeApp (desc type_))
          ~custom_error_message
      | Op (SetProperty { prop; value; lhs; _ }) ->
        let loc_reason =
          if Loc.contains (loc_of_aloc (loc_of_reason lhs)) loc then
            lhs
          else
            value
        in
        root
          ~loc
          ~frames
          ~root_reason:loc_reason
          ~root_message:(RootCannotAssign { init = desc value; target = Some (desc prop) })
          ~custom_error_message
      | Op (UpdateProperty { prop; lhs }) ->
        root
          ~loc
          ~frames
          ~root_reason:lhs
          ~root_message:(RootCannotUpdate (desc prop))
          ~custom_error_message
      | Op (DeleteProperty { prop; lhs }) ->
        root
          ~loc
          ~frames
          ~root_reason:lhs
          ~root_message:(RootCannotDelete (desc prop))
          ~custom_error_message
      | Op (RefinementCheck { test; discriminant }) ->
        root
          ~loc
          ~frames
          ~root_reason:test
          ~root_message:(RootCannotCheckAgainst { test = desc test; discriminant })
          ~custom_error_message
      | Op (SwitchRefinementCheck { test; discriminant }) ->
        let root_loc = loc_of_aloc test in
        root_with_loc_and_specific_loc
          ~loc
          ~frames
          ~root_loc
          ~specific_loc:root_loc
          ~root_message:(RootCannotCheckAgainstSwitchDiscriminant discriminant)
          ~custom_error_message
      | Op (EvalMappedType { mapped_type }) ->
        root
          ~loc
          ~frames
          ~root_reason:mapped_type
          ~root_message:(RootCannotInstantiateEval mapped_type)
          ~custom_error_message
      | Op (TypeGuardIncompatibility { guard_type; param_name }) ->
        root
          ~loc
          ~frames
          ~root_reason:guard_type
          ~root_message:(RootCannotUseTypeGuard { guard_type; param_name })
          ~custom_error_message
      | Op (ComponentRestParamCompatibility { rest_param = _ }) ->
        (* Special-cased incompatibility error. This should be unreachable, but just in case
         * our error messages can compose in a way that would miss the special case we'd rather
         * output a bad error than crash. *)
        unknown_root ~loc ~frames ~custom_error_message
      | Op
          (PositiveTypeGuardConsistency
            {
              reason = _;
              return_reason = return;
              param_reason = param;
              guard_type_reason = guard_type;
              is_return_false_statement;
            }
            ) ->
        let (all_frames, explanations) = frames in
        let frames =
          ( all_frames,
            ExplanationTypeGuardPositiveConsistency
              { return; param; guard_type; is_return_false_statement }
            :: explanations
          )
        in
        root
          ~loc
          ~frames
          ~root_reason:return
          ~root_message:(RootCannotReturn (desc return))
          ~custom_error_message
      | Frame (ReactDeepReadOnly (props_loc, Props), use_op) ->
        explanation
          ~loc
          ~frames
          ~use_op
          ~explanation:(ExplanationReactComponentPropsDeepReadOnly props_loc)
          ~custom_error_message
      | Frame (ReactDeepReadOnly (props_loc, HookArg), use_op) ->
        explanation
          ~loc
          ~frames
          ~use_op
          ~explanation:(ExplanationReactHookArgsDeepReadOnly props_loc)
          ~custom_error_message
      | Frame (ReactDeepReadOnly (hook_loc, HookReturn), use_op) ->
        explanation
          ~loc
          ~frames
          ~use_op
          ~explanation:(ExplanationReactHookReturnDeepReadOnly hook_loc)
          ~custom_error_message
      | Frame (ConstrainedAssignment { name; declaration; providers }, use_op) ->
        explanation
          ~loc
          ~frames
          ~use_op
          ~explanation:(ExplanationConstrainedAssign { name; declaration; providers })
          ~custom_error_message
      | Frame (UnifyFlip, (Frame (ArrayElementCompatibility _, _) as use_op)) ->
        explanation
          ~loc
          ~frames
          ~use_op
          ~explanation:ExplanationArrayInvariantTyping
          ~custom_error_message
      | Frame (ArrayElementCompatibility { lower; upper }, use_op) ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:
            (FrameArrayElement
               { incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
            )
          ~custom_error_message
      | Frame (FunParam { n; lower; upper; name }, (Frame (FunCompatibility _, _) as use_op)) ->
        let arg =
          match name with
          | Some "this" ->
            FrameFunThisParam
              { incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
          | _ ->
            FrameFunNthParam
              { n; incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
        in
        unwrap_frame ~loc ~frames ~frame_reason:lower ~use_op ~frame:arg ~custom_error_message
      | Frame (FunParam { n; lower; upper; name }, use_op) ->
        let incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) in
        let arg =
          match name with
          | Some "this" -> FrameFunThisArgument { incompatibility_pair }
          | _ -> FrameFunNthArgument { n; incompatibility_pair }
        in
        unwrap_frame ~loc ~frames ~frame_reason:lower ~use_op ~frame:arg ~custom_error_message
      | Frame (FunRestParam _, use_op) -> loop ~loc ~frames ~use_op ~custom_error_message
      | Frame (FunReturn { lower; upper }, use_op) ->
        unwrap_frame_without_loc
          ~loc
          ~frames
          ~use_op
          ~frame:
            (FrameReturnValue
               { incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
            )
          ~custom_error_message
      | Frame (IndexerKeyCompatibility { lower; upper }, use_op) ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:
            (FrameIndexerPropertyKey
               { incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
            )
          ~custom_error_message
      | Frame (OpaqueTypeLowerBoundCompatibility { lower; _ }, use_op)
      | Frame (OpaqueTypeUpperBoundCompatibility { lower; _ }, use_op) ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:FrameAnonymous
          ~custom_error_message
      | Frame
          ( OpaqueTypeCustomErrorCompatibility
              { lower; upper; lower_t; upper_t; name; custom_error_loc },
            use_op
          ) ->
        let lower_desc =
          match lower_t with
          | Type.TypeOrTypeDesc.Type _ -> Error (desc_of_reason lower)
          | Type.TypeOrTypeDesc.TypeDesc d -> d
        in
        let upper_desc =
          match upper_t with
          | Type.TypeOrTypeDesc.Type _ -> Error (desc_of_reason upper)
          | Type.TypeOrTypeDesc.TypeDesc d -> d
        in
        let custom_error_message =
          Some
            (MessageIncompatibleGeneralWithPrintedTypes
               {
                 lower_loc = loc_of_reason lower;
                 upper_loc = loc_of_reason upper;
                 lower_desc;
                 upper_desc;
               }
            )
        in
        let frames = ([], [ExplanationCustomError { name; custom_error_loc }]) in
        next_with_loc
          ~loc:(loc_of_aloc (loc_of_reason lower))
          ~frames
          ~frame_reason:lower
          ~use_op
          ~custom_error_message
      | Frame (PropertyCompatibility { prop = None; lower; upper }, use_op) ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:
            (FrameIndexerProperty
               { incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
            )
          ~custom_error_message
      | Frame (PropertyCompatibility { prop = Some (OrdinaryName "$call"); lower; upper }, use_op)
        ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:
            (FrameCallableSignature
               { incompatibility_pair = opt_incompatibility_pair use_op (lower, upper) }
            )
          ~custom_error_message
      | Frame (EnumRepresentationTypeCompatibility { lower; _ }, use_op) ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:FrameEnumRepresentationType
          ~custom_error_message
      | Frame (UnifyFlip, (Frame (PropertyCompatibility _, _) as use_op)) ->
        explanation
          ~loc
          ~frames
          ~use_op
          ~explanation:ExplanationPropertyInvariantTyping
          ~custom_error_message
      | Frame (PropertyCompatibility { prop = Some prop; lower; upper; _ }, use_op) ->
        let lower_loc = loc_of_prop_compatibility_reason loc lower use_op in
        (* Loop through our parent use_op to get our access chain. *)
        let (lower_loc, props, use_op) = loop_to_form_access_chain lower_loc use_op in
        unwrap_frame_with_loc
          ~loc
          ~frames
          ~frame_loc:lower_loc
          ~use_op
          ~frame:
            (FrameAccessChain
               {
                 chain = (PropSegment prop, props);
                 incompatibility_pair = opt_incompatibility_pair use_op (lower, upper);
               }
            )
          ~custom_error_message
      | Frame (TupleElementCompatibility { n; lower; upper; _ }, use_op) ->
        let lower_loc = loc_of_aloc (loc_of_reason lower) in
        (* Loop through our parent use_op to get our access chain. *)
        let (lower_loc, props, use_op) = loop_to_form_access_chain lower_loc use_op in
        unwrap_frame_with_loc
          ~loc
          ~frames
          ~frame_loc:lower_loc
          ~use_op
          ~frame:
            (FrameAccessChain
               {
                 chain = (TupleIndexSegment n, props);
                 incompatibility_pair = opt_incompatibility_pair use_op (lower, upper);
               }
            )
          ~custom_error_message
      | Frame (TypeArgCompatibility { targ; lower; _ }, use_op) ->
        unwrap_frame
          ~loc
          ~frames
          ~frame_reason:lower
          ~use_op
          ~frame:(FrameTypeArgument targ)
          ~custom_error_message
      | Frame (TypeParamBound { name }, use_op) ->
        unwrap_frame_without_loc
          ~loc
          ~frames
          ~use_op
          ~frame:(FrameTypeParameterBound (Subst_name.string_of_subst_name name))
          ~custom_error_message
      | Frame (TypeGuardCompatibility, use_op) ->
        unwrap_frame_without_loc
          ~loc
          ~frames
          ~use_op
          ~frame:FrameTypePredicate
          ~custom_error_message
      | Frame (FunCompatibility { lower; _ }, use_op) ->
        next_with_loc ~loc ~frames ~frame_reason:lower ~use_op ~custom_error_message
      | Frame (OpaqueTypeLowerBound { opaque_t_reason = _ }, use_op) ->
        loop ~loc ~frames ~use_op ~custom_error_message
      | Frame (OpaqueTypeUpperBound { opaque_t_reason = _ }, use_op) ->
        loop ~loc ~frames ~use_op ~custom_error_message
      | Frame (FunMissingArg _, use_op)
      | Frame (ImplicitTypeParam, use_op)
      | Frame (ReactConfigCheck, use_op)
      | Frame (ReactGetConfig _, use_op)
      | Frame (UnifyFlip, use_op)
      | Frame (MappedTypeKeyCompatibility _, use_op)
      | Frame (TupleAssignment _, use_op)
      | Frame (ReactDeepReadOnly (_, DebugAnnot), use_op)
      | Frame (RendersCompatibility, use_op) ->
        loop ~loc ~frames ~use_op ~custom_error_message
    and next_with_loc ~loc ~frames ~frame_reason ~use_op ~custom_error_message =
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
      loop ~loc ~frames ~use_op ~custom_error_message
    and unwrap_frame_with_loc ~loc ~frames ~frame_loc ~use_op ~frame ~custom_error_message =
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
      loop ~loc ~frames ~use_op ~custom_error_message
    and unwrap_frame ~loc ~frames ~frame_reason ~use_op ~frame ~custom_error_message =
      let frame_loc = loc_of_aloc (loc_of_reason frame_reason) in
      unwrap_frame_with_loc ~loc ~frames ~frame_loc ~use_op ~frame ~custom_error_message
    and unwrap_frame_without_loc ~loc ~frames ~use_op ~frame ~custom_error_message =
      (* Same logic as `unwrap_frame` except we don't have a frame location. *)
      let (all_frames, explanations) = frames in
      let frames = (frame :: all_frames, explanations) in
      loop ~loc ~frames ~use_op ~custom_error_message
    and explanation ~loc ~frames ~use_op ~(explanation : 'loc explanation) ~custom_error_message =
      let (all_frames, explanations) = frames in
      let frames = (all_frames, explanation :: explanations) in
      loop ~loc ~frames ~use_op ~custom_error_message
    and unknown_root ~loc ~frames ~custom_error_message =
      (* We don't know what our root is! Return what we do know. *)
      let (all_frames, explanations) = frames in
      (None, custom_error_message, loc, all_frames, explanations)
    and root_with_loc_and_specific_loc
        ~loc ~frames ~root_loc ~specific_loc ~root_message ~custom_error_message =
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
      (Some (root_loc, root_message), custom_error_message, loc, all_frames, explanations)
    and root_with_specific_reason
        ~loc ~frames ~root_reason ~specific_reason ~root_message ~custom_error_message =
      let root_loc = loc_of_aloc (loc_of_reason root_reason) in
      let specific_loc = loc_of_aloc (loc_of_reason specific_reason) in
      root_with_loc_and_specific_loc
        ~loc
        ~frames
        ~root_loc
        ~specific_loc
        ~root_message
        ~custom_error_message
    and root ~loc ~frames ~root_reason ~root_message ~custom_error_message =
      let root_loc = loc_of_aloc (loc_of_reason root_reason) in
      root_with_loc_and_specific_loc
        ~loc
        ~frames
        ~root_loc
        ~specific_loc:root_loc
        ~root_message
        ~custom_error_message
    in
    (fun loc use_op -> loop ~loc ~frames:([], []) ~use_op ~custom_error_message:None)
  in
  (* Make a friendly error based on a use_op. The message we are provided should
   * not have any punctuation. Punctuation will be provided after the frames of
   * an error message. *)
  let mk_use_op_error loc use_op ?explanation message =
    let (root, custom_error_msg, loc, frames, explanations) =
      unwrap_use_ops (loc_of_aloc loc) use_op
    in
    let code = Flow_error.code_of_error error in
    let explanations =
      Base.Option.value_map ~f:(fun x -> x :: explanations) ~default:explanations explanation
    in
    let message = Base.Option.value ~default:message custom_error_msg in
    mk_error ?root ~frames ~explanations loc code message
  in
  let mk_use_op_error_reason reason use_op ?explanation message =
    mk_use_op_error (loc_of_reason reason) use_op ?explanation message
  in
  let mk_no_frame_or_explanation_error reason message =
    let loc = loc_of_aloc (loc_of_reason reason) in
    let code = Flow_error.code_of_error error in
    mk_error ~frames:[] ~explanations:[] loc code message
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
      let (root, custom_error_msg, loc, frames, explanations) =
        unwrap_use_ops (loc_of_aloc loc) use_op
      in
      let error_code = Flow_error.code_of_error error in
      (match custom_error_msg with
      | Some custom_error_msg ->
        mk_error ?root ~frames ~explanations loc error_code custom_error_msg
      | None ->
        let speculation_errors =
          Base.List.map
            ~f:(fun msg ->
              let score = score_of_msg msg in
              let error =
                Flow_error.error_of_msg ~source_file msg
                |> make_intermediate_error ~loc_of_aloc ~speculation:true
              in
              (score, error))
            branches
        in
        mk_speculation_error ~loc ~root ~frames ~explanations ~error_code speculation_errors)
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
    let upper = mod_upper_reason_according_to_use_ops upper use_op in
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
        if loc_of_reason upper = loc_of_reason return then
          make_error
            lower
            (MessageIncompatibleImplicitReturn { lower; upper = desc_of_reason upper })
        else
          make_error lower (MessageIncompatibleGeneral { lower; upper })
      | ComponentRestParamCompatibility { rest_param } ->
        mk_no_frame_or_explanation_error
          rest_param
          (MessageIncompatibleComponentRestParam rest_param)
      (* Default incompatibility. *)
      | _ -> begin make_error lower (MessageIncompatibleGeneral { lower; upper }) end)
  in
  (* An error between two incompatible types due to invariant subtyping.
   *
   * This is a specialization of mk_incompatible_use_error. *)
  let mk_incompatible_invariant_subtyping_error
      ?additional_explanation ~sub_component ~lower_loc ~upper_loc ~lower_desc ~upper_desc use_op =
    let make_error loc message =
      mk_use_op_error loc use_op ?explanation:additional_explanation message
    in
    make_error
      lower_loc
      (MessageIncompatibleDueToInvariantSubtyping
         { sub_component; lower_loc; upper_loc; lower_desc; upper_desc }
      )
  in
  let mk_prop_missing_in_lookup_error loc prop lower use_op suggestion reason_indexer =
    let lower = mod_lower_reason_according_to_use_ops lower use_op in
    mk_use_op_error
      loc
      use_op
      (MessagePropMissing { lower; upper = None; prop; suggestion; reason_indexer })
  in
  let mk_prop_missing_in_subtyping_error prop suggestion lower upper reason_indexer use_op =
    let loc = loc_of_reason lower in
    let lower = mod_lower_reason_according_to_use_ops lower use_op in
    mk_use_op_error
      loc
      use_op
      (MessagePropMissing { lower; upper = Some upper; prop; suggestion; reason_indexer })
  in
  let mk_props_missing_in_subtyping_error props lower upper use_op =
    let loc = loc_of_reason lower in
    let lower = mod_lower_reason_according_to_use_ops lower use_op in
    let explanation = None in
    mk_use_op_error loc use_op ?explanation (MessagePropsMissing { lower; upper; props })
  in
  let mk_props_missing_in_invariant_subtyping_error
      ~props
      ~reason_lower
      ~reason_upper
      ~lower_obj_loc
      ~upper_obj_loc
      ~lower_obj_desc
      ~upper_obj_desc
      ~use_op =
    let reason_lower = mod_lower_reason_according_to_use_ops reason_lower use_op in
    let explanation =
      let props_plural =
        match props with
        | (_, []) -> false
        | (_, _ :: _) -> true
      in
      Some
        (ExplanationPropertyMissingDueToNeutralOptionalProperty
           {
             props_plural;
             lower_obj_loc;
             upper_obj_loc;
             lower_obj_desc;
             upper_obj_desc;
             upper_object_reason = reason_upper;
           }
        )
    in
    mk_use_op_error
      (loc_of_reason reason_lower)
      use_op
      ?explanation
      (MessagePropsMissing { lower = reason_lower; upper = reason_upper; props })
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
    let use_op_error = mk_use_op_error use_loc use_op in
    match use_kind with
    | IncompatibleElemTOfArrT -> use_op_error (MessageLowerIsNotArrayIndex lower)
    | IncompatibleGetPrivatePropT
    | IncompatibleSetPrivatePropT ->
      mk_use_op_error use_loc use_op (MessageLowerIsNotClassWithPrivateProps lower)
    | IncompatibleMixedCallT -> mk_use_op_error use_loc use_op (MessageUnknownParameterTypes lower)
    | IncompatibleCallT -> mk_use_op_error use_loc use_op (MessageLowerIsNotFunction lower)
    | IncompatibleObjAssignFromTSpread
    | IncompatibleArrRestT ->
      mk_use_op_error use_loc use_op (MessageLowerIsNotArray lower)
    | IncompatibleObjAssignFromT
    | IncompatibleObjRestT
    | IncompatibleGetKeysT
    | IncompatibleGetValuesT ->
      mk_use_op_error use_loc use_op (MessageLowerIsNotObject lower)
    | IncompatibleMapTypeTObject ->
      mk_use_op_error use_loc use_op (MessageInvalidArgument { lower; upper })
    | IncompatibleMixinT
    | IncompatibleThisSpecializeT ->
      mk_use_op_error use_loc use_op (MessageLowerIsNotClass lower)
    | IncompatibleSpecializeT
    | IncompatibleVarianceCheckT ->
      mk_use_op_error use_loc use_op (MessageLowerIsNotPolymorphicType lower)
    | IncompatibleSuperT -> mk_use_op_error use_loc use_op (MessageLowerIsNotInheritable lower)
    | IncompatibleGetPropT (prop_loc, prop)
    | IncompatibleSetPropT (prop_loc, prop)
    | IncompatibleHasOwnPropT (prop_loc, prop)
    | IncompatibleMethodT (prop_loc, prop) ->
      mk_prop_missing_in_lookup_error
        prop_loc
        (Base.Option.map ~f:display_string_of_name prop)
        lower
        use_op
        None
        None
    | IncompatibleGetElemT prop_loc
    | IncompatibleSetElemT prop_loc
    | IncompatibleCallElemT prop_loc ->
      mk_prop_missing_in_lookup_error prop_loc None lower use_op None None
    | IncompatibleGetStaticsT -> mk_use_op_error use_loc use_op (MessageLowerIsNotInstanceType lower)
    | IncompatibleBindT -> mk_use_op_error use_loc use_op (MessageLowerIsNotFunctionType lower)
    (* unreachable or unclassified use-types. until we have a mechanical way
       to verify that all legit use types are listed above, we can't afford
       to throw on a use type, so mark the error instead *)
    | IncompatibleUnclassified ctor ->
      mk_use_op_error use_loc use_op (MessageLowerIsNotSupportedByUnclassifiedUse { lower; ctor })
  in
  (* When an object property has a polarity that is incompatible with another
   * error then we create one of these errors. We use terms like "read-only" and
   * "write-only" to better reflect how the user thinks about these properties.
   * Other terminology could include "contravariant", "covariant", and
   * "invariant". Generally these terms are impenatrable to the average
   * JavaScript developer. If we had more documentation explaining these terms
   * it may be fair to use them in error messages. *)
  let mk_prop_polarity_mismatch_error lower upper props use_op =
    let use_op =
      match use_op with
      | Frame (PropertyCompatibility c, use_op)
        when Nel.exists
               (fun (prop, _, _) -> Base.Option.map ~f:display_string_of_name c.prop = prop)
               props ->
        use_op
      | _ -> use_op
    in
    mk_use_op_error_reason lower use_op (MessagePropPolarityMismatch { lower; upper; props })
  in

  let intermediate_error =
    match (loc, friendly_message_of_msg msg) with
    | (Some loc, Error_message.Normal message) ->
      mk_error ~kind (loc_of_aloc loc) (Flow_error.code_of_error error) message
    | (None, UseOp { loc; message; use_op; explanation }) ->
      mk_use_op_error loc use_op ?explanation message
    | (None, PropMissingInLookup { loc; prop; reason_obj; use_op; suggestion; reason_indexer }) ->
      mk_prop_missing_in_lookup_error loc prop reason_obj use_op suggestion reason_indexer
    | ( None,
        PropMissingInSubtyping
          { prop; reason_lower; reason_upper; reason_indexer; suggestion; use_op }
      ) ->
      mk_prop_missing_in_subtyping_error
        prop
        suggestion
        reason_lower
        reason_upper
        reason_indexer
        use_op
    | (None, PropsMissingInSubtyping { props; reason_lower; reason_upper; use_op }) ->
      mk_props_missing_in_subtyping_error props reason_lower reason_upper use_op
    | ( None,
        PropsMissingInInvariantSubtyping
          {
            props;
            reason_lower;
            reason_upper;
            lower_obj_loc;
            upper_obj_loc;
            lower_obj_desc;
            upper_obj_desc;
            use_op;
          }
      ) ->
      mk_props_missing_in_invariant_subtyping_error
        ~props
        ~reason_lower
        ~reason_upper
        ~lower_obj_loc
        ~upper_obj_loc
        ~lower_obj_desc
        ~upper_obj_desc
        ~use_op
    | ( None,
        PropsExtraAgainstExactObject { props; reason_l_obj = lower; reason_r_obj = upper; use_op }
      ) ->
      mk_use_op_error
        (loc_of_reason lower)
        use_op
        (MessagePropExtraAgainstExactObject { lower; upper; props })
    | (None, PropPolarityMismatch { reason_lower; reason_upper; props; use_op }) ->
      mk_prop_polarity_mismatch_error reason_lower reason_upper props use_op
    | (None, IncompatibleUse { loc; upper_kind; reason_lower; reason_upper; use_op }) ->
      mk_incompatible_use_error loc upper_kind reason_lower reason_upper use_op
    | (None, IncompatibleSubtyping { reason_lower; reason_upper; use_op; explanation }) ->
      mk_incompatible_error ?additional_explanation:explanation reason_lower reason_upper use_op
    | ( None,
        IncompatibleInvariantSubtyping
          { sub_component; lower_loc; upper_loc; lower_desc; upper_desc; use_op; explanation }
      ) ->
      mk_incompatible_invariant_subtyping_error
        ?additional_explanation:explanation
        ~sub_component
        ~lower_loc
        ~upper_loc
        ~lower_desc
        ~upper_desc
        use_op
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
      get_ast:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
      strip_root:File_path.t option ->
      'loc intermediate_error ->
      Loc.t Flow_errors_utils.printable_error =
 fun ~loc_of_aloc ~get_ast ~strip_root intermediate_error ->
  let open Flow_errors_utils in
  let text = Friendly.text in
  let code = Friendly.code in
  let ref = Friendly.ref_map loc_of_aloc in
  let no_desc_ref = Flow_errors_utils.Friendly.no_desc_ref_map loc_of_aloc in
  let hardcoded_string_desc_ref desc loc =
    Flow_errors_utils.Friendly.hardcoded_string_desc_ref desc (loc_of_aloc loc)
  in
  let ref_of_ty_or_desc loc desc =
    match desc with
    | Ok ty ->
      let ty = Ty_utils.simplify_type ~merge_kinds:true ty in
      let ty_str = Ty_printer.string_of_t_single_line ~ts_syntax:true ty in
      let ty_str =
        if Base.String.length ty_str > 100 then
          String.sub ty_str 0 100 ^ "..."
        else
          ty_str
      in
      let desc = spf "`%s`" ty_str in
      hardcoded_string_desc_ref desc loc
    | Error desc -> ref (mk_reason desc loc)
  in
  let desc = Friendly.desc_of_reason_desc in
  let msg_export prefix export_name =
    if export_name = "default" then
      (text "", text "the default export")
    else
      (text prefix, code export_name)
  in
  let msg_of_invalid_obj_key_kind kind =
    match kind with
    | InvalidObjKey.Other -> None
    | InvalidObjKey.NumberNonInt -> Some [text " Only integer-like number literals are allowed."]
    | InvalidObjKey.NumberTooLarge ->
      Some
        [text " Number literals must not be larger than "; code "Number.MAX_SAFE_INTEGER"; text "."]
    | InvalidObjKey.NumberTooSmall ->
      Some
        [
          text " Number literals must not be smaller than "; code "Number.MIN_SAFE_INTEGER"; text ".";
        ]
  in
  let mk_tuple_element_error_message loc_of_aloc ~reason ~index ~name kind =
    let open Flow_errors_utils.Friendly in
    let index_ref =
      Reference ([Code (string_of_int index)], loc_of_aloc (def_loc_of_reason reason))
    in
    let label =
      Base.Option.value_map name ~default:[] ~f:(fun name -> [text " labeled "; code name])
    in
    [text "tuple element at index "; index_ref] @ label @ [text " is not "; text kind]
  in
  let explanation_to_friendly_msgs = function
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
    | ExplanationFunctionsWithStaticsToObject ->
      [text "Functions without statics are not compatible with objects"]
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
    | ExplanationPropertyMissingDueToNeutralOptionalProperty
        {
          props_plural;
          lower_obj_loc;
          upper_obj_loc;
          lower_obj_desc;
          upper_obj_desc;
          upper_object_reason;
        } ->
      ( if props_plural then
        [text "These optional properties of "; ref upper_object_reason; text " are"]
      else
        [text "This optional property of "; ref upper_object_reason; text " is"]
      )
      @ [text " invariantly typed. To fix,\n- Either "]
      @ (match (lower_obj_desc, upper_obj_desc) with
        | (Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc), Ok _)
          ->
          [
            text "annotate ";
            ref (mk_reason desc lower_obj_loc);
            text " with ";
            ref_of_ty_or_desc upper_obj_loc upper_obj_desc;
          ]
        | (Ok _, Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc))
          ->
          [
            text "annotate ";
            ref (mk_reason desc upper_obj_loc);
            text " with ";
            ref_of_ty_or_desc lower_obj_loc lower_obj_desc;
          ]
        | _ ->
          [
            text "make ";
            ref_of_ty_or_desc lower_obj_loc lower_obj_desc;
            text " and ";
            ref_of_ty_or_desc upper_obj_loc upper_obj_desc;
            text " exactly the same";
          ])
      @ [text "\n- Or make "]
      @ [
          ref upper_object_reason;
          text " readonly. ";
          text "See ";
          text
            "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number";
        ]
    | ExplanationIncompatibleReactDeepReadOnly -> [text "Consider using "; code "React.Immutable<>"]
    | ExplanationInvariantSubtypingDueToMutableArray
        { lower_array_loc; upper_array_loc; lower_array_desc; upper_array_desc; upper_array_reason }
      ->
      [
        text "The above-mentioned two types must be the same because arrays are invariantly typed. ";
        text "To fix the error,\n- Either ";
      ]
      @ (match (lower_array_desc, upper_array_desc) with
        | (Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc), Ok _)
          ->
          [
            text "annotate ";
            ref (mk_reason desc lower_array_loc);
            text " with ";
            ref_of_ty_or_desc upper_array_loc upper_array_desc;
          ]
        | (Ok _, Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc))
          ->
          [
            text "annotate ";
            ref (mk_reason desc upper_array_loc);
            text " with ";
            ref_of_ty_or_desc lower_array_loc lower_array_desc;
          ]
        | _ ->
          [
            text "make ";
            ref_of_ty_or_desc lower_array_loc lower_array_desc;
            text " and ";
            ref_of_ty_or_desc upper_array_loc upper_array_desc;
            text " exactly the same";
          ])
      @ [
          text "\n- Or make ";
          ref upper_array_reason;
          text " a ";
          code "ReadonlyArray";
          text ".\n";
          text "See ";
          text
            "https://flow.org/en/docs/faq/#why-cant-i-pass-an-arraystring-to-a-function-that-takes-an-arraystring-number";
        ]
    | ExplanationInvariantSubtypingDueToMutableProperty
        {
          lower_obj_loc;
          upper_obj_loc;
          lower_obj_desc;
          upper_obj_desc;
          upper_object_reason;
          property_name;
        } ->
      let prop =
        match property_name with
        | None -> [text "the indexer"]
        | Some name -> [text "property "; code name]
      in
      [text "The above-mentioned two types must be the same because "]
      @ prop
      @ [text " is invariantly typed. To fix the error,\n- Either "]
      @ (match (lower_obj_desc, upper_obj_desc) with
        | (Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc), Ok _)
          ->
          [
            text "annotate ";
            ref (mk_reason desc lower_obj_loc);
            text " with ";
            ref_of_ty_or_desc upper_obj_loc upper_obj_desc;
          ]
        | (Ok _, Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc))
          ->
          [
            text "annotate ";
            ref (mk_reason desc upper_obj_loc);
            text " with ";
            ref_of_ty_or_desc lower_obj_loc lower_obj_desc;
          ]
        | _ ->
          [
            text "make ";
            ref_of_ty_or_desc lower_obj_loc lower_obj_desc;
            text " and ";
            ref_of_ty_or_desc upper_obj_loc upper_obj_desc;
            text " exactly the same";
          ])
      @ [text "\n- Or make "]
      @ prop
      @ [
          text " in ";
          ref upper_object_reason;
          text " readonly. ";
          text "See ";
          text
            "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number";
        ]
    | ExplanationInvariantSubtypingDueToMutableProperties
        {
          lower_obj_loc;
          upper_obj_loc;
          lower_obj_desc;
          upper_obj_desc;
          upper_object_reason;
          properties;
        } ->
      let props_msg =
        text "properties "
        :: (Base.List.map properties ~f:(fun prop -> [code (display_string_of_name prop)])
           |> Flow_errors_utils.Friendly.conjunction_concat
           )
      in
      [text "The above-mentioned two types must be the same because "]
      @ props_msg
      @ [text " are invariantly typed. To fix the error,\n- Either "]
      @ (match (lower_obj_desc, upper_obj_desc) with
        | (Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc), Ok _)
          ->
          [
            text "annotate ";
            ref (mk_reason desc lower_obj_loc);
            text " with ";
            ref_of_ty_or_desc upper_obj_loc upper_obj_desc;
          ]
        | (Ok _, Error ((RObjectLit | RObjectLit_UNSOUND | RArrayLit | RArrayLit_UNSOUND) as desc))
          ->
          [
            text "annotate ";
            ref (mk_reason desc upper_obj_loc);
            text " with ";
            ref_of_ty_or_desc lower_obj_loc lower_obj_desc;
          ]
        | _ ->
          [
            text "make ";
            ref_of_ty_or_desc lower_obj_loc lower_obj_desc;
            text " and ";
            ref_of_ty_or_desc upper_obj_loc upper_obj_desc;
            text " exactly the same";
          ])
      @ [text "\n- Or make "]
      @ props_msg
      @ [
          text " in ";
          ref upper_object_reason;
          text " readonly. ";
          text "See ";
          text
            "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number";
        ]
    | ExplanationTypeGuardPositiveConsistency
        { return; param; guard_type; is_return_false_statement } ->
      [
        text "The type of ";
        ref param;
        text " at the return expression ";
        ref return;
        text " needs to be compatible with the guard type ";
        ref guard_type;
        text ".";
        text " See 1. in ";
        text
          "https://flow.org/en/docs/types/type-guards/#toc-consistency-checks-of-type-guard-functions";
      ]
      @
      if is_return_false_statement then
        [
          text ". Consider replacing the body of this predicate ";
          text "function with a single conditional expression";
        ]
      else
        []
    | ExplanationConstrainedAssign { name; declaration; providers } ->
      let assignments =
        match providers with
        | [] -> (* should not happen *) [text "one of its initial assignments"]
        | [l] when Loc.equal (loc_of_aloc l) (loc_of_aloc declaration) ->
          [text "its "; hardcoded_string_desc_ref "initializer" declaration]
        | [l] -> [text "its "; hardcoded_string_desc_ref "initial assignment" l]
        | providers ->
          text "one of its initial assignments"
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
    | ExplanationCustomError { name; custom_error_loc } ->
      let loc = loc_of_aloc custom_error_loc in
      let custom_docs =
        Loc.source loc
        |> Base.Option.bind ~f:get_ast
        |> Base.Option.bind ~f:(fun (_, { Flow_ast.Program.all_comments; _ }) ->
               Base.List.find_map
                 all_comments
                 ~f:(fun (comment_loc, { Flow_ast.Comment.kind; text; _ }) ->
                   if Loc.equal loc comment_loc && kind = Flow_ast.Comment.Block then
                     Jsdoc.parse text
                   else
                     None
               )
           )
        |> Base.Option.bind ~f:(fun jsdoc ->
               let description =
                 jsdoc |> Jsdoc.description |> Base.Option.map ~f:Base.String.strip
               in
               let example =
                 Base.List.Assoc.find
                   (Jsdoc.unrecognized_tags jsdoc)
                   ~equal:Base.String.equal
                   "example"
                 |> Base.Option.bind ~f:Base.Fn.id
                 |> Base.Option.map ~f:Base.String.strip
               in
               match (description, example) with
               | (None, None) -> None
               | (Some d, None) -> Some [text "Description of "; code name; text ": "; text d]
               | (None, Some e) -> Some [text "Example for "; code name; text ": "; code e]
               | (Some d, Some e) ->
                 Some
                   [
                     text "Description of ";
                     code name;
                     text ": ";
                     text d;
                     text ". Example for ";
                     code name;
                     text ": ";
                     code e;
                   ]
           )
      in
      (match custom_docs with
      | Some m -> m
      | None -> [text "See "; hardcoded_string_desc_ref "relevant docs" custom_error_loc])
    | ExplanationReactComponentPropsDeepReadOnly props_loc ->
      [
        text "React ";
        hardcoded_string_desc_ref "component properties" props_loc;
        text " and their nested props and elements cannot be written to. ";
        text "(https://react.dev/reference/rules/components-and-hooks-must-be-pure#props)";
      ]
    | ExplanationReactHookArgsDeepReadOnly props_loc ->
      [
        text "React ";
        hardcoded_string_desc_ref "hook arguments" props_loc;
        text " and their nested elements cannot be written to. ";
        text
          "(https://react.dev/reference/rules/components-and-hooks-must-be-pure#return-values-and-arguments-to-hooks-are-immutable)";
      ]
    | ExplanationReactHookIncompatibleWithEachOther ->
      [
        text
          "Different React hooks are not compatible with each other, because hooks cannot be called conditionally";
      ]
    | ExplanationReactHookIncompatibleWithNormalFunctions ->
      [
        text
          "React hooks and other functions are not compatible with each other, because hooks cannot be called conditionally";
      ]
    | ExplanationReactHookReturnDeepReadOnly hook_loc ->
      [
        text "The return value of a ";
        hardcoded_string_desc_ref "React hook" hook_loc;
        text " cannot be written to. ";
        text
          "(https://react.dev/reference/rules/components-and-hooks-must-be-pure#return-values-and-arguments-to-hooks-are-immutable)";
      ]
    | ExplanationAdditionalUnionMembers { left; right; members; extra_number } ->
      [text "Type "; ref left; text " includes members "]
      @ (Base.List.map ~f:(fun m -> code m) members |> Base.List.intersperse ~sep:(text ", "))
      @ ( if extra_number > 0 then
          [text (spf " and %d more" extra_number)]
        else
          []
        )
      @ [text " that are not included in type "; ref right]
    | ExplanationObjectLiteralNeedsRecordSyntax { record_name; obj_reason } ->
      [
        text "Fix by adding the record name ";
        code record_name;
        text " before the ";
        ref obj_reason;
        text ", e.g. ";
        code (spf "%s {...}" record_name);
      ]
  in
  let frame_to_friendly_msgs ~include_incompatibility_pair =
    let map =
      if include_incompatibility_pair then
        Base.Option.map ~f:(fun (l, u) -> [ref l; text " is incompatible with "; ref u])
      else
        Base.Fn.const None
    in
    function
    | FrameAnonymous -> (None, [])
    | FrameAccessChain { chain = (last, _) as chain; incompatibility_pair } ->
      let kind =
        match last with
        | PropSegment _ -> "property "
        | TupleIndexSegment _ -> "index "
      in
      let (first, rest) = Nel.rev chain in
      ( map incompatibility_pair,
        [
          text kind;
          code
            (List.fold_left
               (fun acc -> function
                 | PropSegment prop -> Utils_js.spf "%s.%s" acc (display_string_of_name prop)
                 | TupleIndexSegment n -> Utils_js.spf "%s[%d]" acc n)
               (match first with
               | PropSegment prop -> display_string_of_name prop
               | TupleIndexSegment n -> string_of_int n)
               rest
            );
        ]
      )
    | FrameArrayElement { incompatibility_pair } ->
      (map incompatibility_pair, [text "array element"])
    | FrameCallableSignature { incompatibility_pair } ->
      (map incompatibility_pair, [text "the callable signature"])
    | FrameEnumRepresentationType -> (None, [text "the enum's representation type"])
    | FrameFunNthArgument { n; incompatibility_pair } ->
      (map incompatibility_pair, [text "the "; text (Utils_js.ordinal n); text " argument"])
    | FrameFunThisArgument { incompatibility_pair } ->
      (map incompatibility_pair, [text "the "; code "this"; text " argument"])
    | FrameFunNthParam { n; incompatibility_pair } ->
      (map incompatibility_pair, [text "the "; text (Utils_js.ordinal n); text " parameter"])
    | FrameFunThisParam { incompatibility_pair } ->
      (map incompatibility_pair, [text "the "; code "this"; text " parameter"])
    | FrameIndexerProperty { incompatibility_pair } ->
      (map incompatibility_pair, [text "the indexer property"])
    | FrameIndexerPropertyKey { incompatibility_pair } ->
      (map incompatibility_pair, [text "the indexer property's key"])
    | FrameTypeArgument targ -> (None, [text "type argument "; ref targ])
    | FrameTypeParameterBound name -> (None, [text "type argument "; code name])
    | FrameTypePredicate -> (None, [text "the type predicate"])
    | FrameReturnValue { incompatibility_pair } ->
      (map incompatibility_pair, [text "the return value"])
  in
  let root_msg_to_root_kind_and_friendly_msgs = function
    | RootCannotAccessIndex { index; object_ } ->
      (OperationRoot, [text "Cannot access "; desc index; text " on "; desc object_])
    | RootCannotAddComputedProperty -> (OperationRoot, [text "Cannot add computed property"])
    | RootCannotAssign { init; target = None } ->
      (OperationRoot, [text "Cannot assign "; desc init; text " to variable"])
    | RootCannotAssign { init; target = Some target } ->
      (OperationRoot, [text "Cannot assign "; desc init; text " to "; desc target])
    | RootCannotCall fn -> (OperationRoot, [text "Cannot call "; desc fn])
    | RootCannotCallWithNamedParam { fn; lower; name } ->
      ( OperationRoot,
        [text "Cannot call "; desc fn; text " with "; desc lower; text " bound to "; code name]
      )
    | RootCannotCallWithNthParam { fn; lower; n : int } ->
      ( OperationRoot,
        [
          text "Cannot call ";
          desc fn;
          text " with ";
          desc lower;
          text " bound to ";
          text (spf "the %s parameter" (Utils_js.ordinal n));
        ]
      )
    | RootCannotCallObjectAssign op ->
      (OperationRoot, [text "Incorrect arguments passed to "; desc op])
    | RootCannotCast { lower; upper } ->
      (OperationRoot, [text "Cannot cast "; desc lower; text " to "; desc upper])
    | RootCannotCheckAgainst { test; discriminant } ->
      (OperationRoot, [text "Invalid check of "; desc test; text " against "; ref discriminant])
    | RootCannotCheckAgainstSwitchDiscriminant discriminant_loc ->
      ( OperationRoot,
        [
          text "Invalid check of case test against ";
          hardcoded_string_desc_ref "switch discriminant" discriminant_loc;
        ]
      )
    | RootCannotCoerce { from; target } ->
      (OperationRoot, [text "Cannot coerce "; desc from; text " to "; desc target])
    | RootCannotConformToCommonInterface { originate_from_import = true } ->
      ( OperationRoot,
        [
          text "The import resolves to a forked module that has implementations of conflicting types";
        ]
      )
    | RootCannotConformToCommonInterface { originate_from_import = false } ->
      (OperationRoot, [text "Cannot conform to common interface module"])
    | RootCannotCreateElement component ->
      (OperationRoot, [text "Cannot create "; desc component; text " element"])
    | RootCannotCreateRecord constructor ->
      (OperationRoot, [text "Cannot create record "; desc constructor])
    | RootCannotDeclareRef ->
      ( ShortExplanationRoot,
        [text "The "; code "ref"; text " parameter must be a subtype of "; code "React.RefSetter"]
      )
    | RootCannotDeclareTypeGuard { type_guard_loc; fn } ->
      ( OperationRoot,
        [
          text "Cannot declare a ";
          hardcoded_string_desc_ref "type guard" type_guard_loc;
          text " for ";
          ref fn;
        ]
      )
    | RootCannotDefineClassMethod { method_; name } ->
      (OperationRoot, [text "Cannot define "; ref method_; text " on "; desc name])
    | RootCannotDefineShadowedProtoProperty ->
      (OperationRoot, [text "Cannot define shadowed proto property"])
    | RootCannotDelete prop -> (OperationRoot, [text "Cannot delete "; desc prop])
    | RootCannotExpectImplicitReturn { upper; fn } ->
      (OperationRoot, [text "Cannot expect "; desc upper; text " as the return type of "; desc fn])
    | RootCannotExtendClass { extends; def } ->
      (OperationRoot, [text "Cannot extend "; ref extends; text " with "; desc def])
    | RootCannotGetProp prop -> (OperationRoot, [text "Cannot get "; desc prop])
    | RootCannotGetRest op -> (OperationRoot, [text "Cannot get rest of "; desc op])
    | RootCannotImplementClass { implements; def } ->
      (OperationRoot, [text "Cannot implement "; ref implements; text " with "; desc def])
    | RootCannotInitializeField { field; body } ->
      (OperationRoot, [text "Cannot initialize "; desc field; text " with "; desc body])
    | RootCannotInstantiateEval type_ -> (OperationRoot, [text "Cannot instantiate "; ref type_])
    | RootCannotInstantiateTypeApp type_ -> (OperationRoot, [text "Cannot instantiate "; desc type_])
    | RootCannotInstantiateRenderType ->
      ( ShortExplanationRoot,
        [
          text "Render types must be a subtype of ";
          code "React.Node";
          text " or a reference to a component-syntax component";
        ]
      )
    | RootCannotReturn value -> (OperationRoot, [text "Cannot return "; desc value])
    | RootCannotShadowProto proto -> (OperationRoot, [text "Cannot shadow proto "; ref proto])
    | RootCannotShadowProtoProperty -> (OperationRoot, [text "Cannot shadow proto property"])
    | RootCannotSpread op -> (OperationRoot, [text "Cannot spread "; desc op])
    | RootCannotUpdate prop -> (OperationRoot, [text "Cannot update "; desc prop])
    | RootCannotUseInferTypeBound { infer } ->
      ( ShortExplanationRoot,
        [text "The infer type "; desc infer; text " must have consistent bounds"]
      )
    | RootCannotUseTypeGuard { guard_type; param_name } ->
      ( ShortExplanationRoot,
        [
          text "The type predicate ";
          ref guard_type;
          text " needs to be compatible with parameter `";
          text param_name;
          text "`'s type";
        ]
      )
    | RootCannotYield value -> (OperationRoot, [text "Cannot yield "; desc value])
  in
  let msg_to_friendly_msgs = function
    | MessagePlainTextReservedForInternalErrorOnly s -> [text s]
    | MessageAlreadyExhaustivelyCheckOneEnumMember { prev_check_loc; enum_reason; member_name } ->
      [
        text "Invalid exhaustive check: ";
        text "case checks for enum member ";
        code member_name;
        text " of ";
        ref enum_reason;
        text ", but member ";
        code member_name;
        text " was already checked at ";
        hardcoded_string_desc_ref "case" prev_check_loc;
        text ".";
      ]
    | MessageAlreadyExhaustivelyCheckAllEnumMembers { enum_reason } ->
      [
        text "Invalid exhaustive check: ";
        text "default case checks for additional enum members of ";
        ref enum_reason;
        text ", but all of its members have already been checked.";
      ]
    | MessageAmbiguousNumericKeyWithVariance ->
      [
        text "Cannot mix number literal keys and variance annotations, ";
        text "as the variance annotation could be interpreted as negating or ";
        text "making positive the number literal. ";
        text "Consider using a string literal key name to disambiguate.";
      ]
    | MessageAmbiguousObjectType ->
      [
        text "Please write this object type as explicitly exact (use ";
        code "{|";
        text " and ";
        code "|}";
        text " instead of ";
        code "{";
        text " and ";
        code "}";
        text ") or as explicitly inexact (add ";
        code "...";
        text " to the end of the list of properties).";
      ]
    | MessageAnyValueUsedAsType use ->
      [
        text "Cannot use ";
        desc use;
        text " as a type because it is an ";
        code "any";
        text "-typed value. ";
        text "Type ";
        desc use;
        text " properly, so it is no longer ";
        code "any";
        text "-typed, to use it as an annotation.";
      ]
    | MessageBadLibdefModuleOverride x ->
      [
        text "This module declaration overrides an existing module ";
        ref x;
        text ". Overriding in library definitions can lead to surprising behaviors.";
      ]
    | MessageBadLibdefNameOverride x ->
      [
        text "This name declaration overrides an existing binding ";
        ref x;
        text ". Overriding in library definitions can lead to surprising behaviors.";
      ]
    | MessageCannotAccessEnumMember { member_name; suggestion; description; enum_reason } ->
      [text "Cannot access "; desc description]
      @
      (match member_name with
      | Some name ->
        let name = display_string_of_name name in
        [text " because "; code name; text " is not a member of "; ref enum_reason; text "."]
        @ Base.Option.value_map suggestion ~default:[] ~f:(fun suggestion ->
              [text " Did you mean the member "; code suggestion; text "?"]
          )
      | None ->
        [text " on "; ref enum_reason; text " because computed access is not allowed on enums."])
    | MessageCannotAccessObjectWithComputedProp { reason_obj; reason_prop; kind } ->
      let suffix = Base.Option.value ~default:[] (msg_of_invalid_obj_key_kind kind) in
      [
        text "Cannot access ";
        ref reason_obj;
        text " with computed property using ";
        ref reason_prop;
        text ".";
      ]
      @ suffix
    | MessageCannotAccessReactRefInRender { usage; in_hook } ->
      let context =
        if in_hook then
          text "within hooks"
        else
          text "during render"
      in
      [
        text "Cannot read ";
        code "current";
        text " from ";
        ref usage;
        text " because ";
        code "ref";
        text " values may not be read ";
        context;
        text ". (https://react.dev/reference/react/useRef).";
      ]
    | MessageCannotAddComputedPropertyDueToPotentialOverwrite { key_loc; overwritten_locs } ->
      [
        text "Cannot add computed property because the indexer";
        no_desc_ref key_loc;
        text " may overwrite properties with explicit keys";
      ]
      @ Base.List.map overwritten_locs ~f:(fun l -> no_desc_ref l)
      @ [text " in a way that Flow cannot track."]
    | MessageCannotApplyNonPolymorphicType ->
      [text "Cannot apply type because it is not a polymorphic type."]
    | MessageCannotAssignToInvalidLHS -> [text "Invalid left-hand side in assignment expression."]
    | MessageCannotAssignToObjectWithComputedProp reason_prop ->
      [
        text "Cannot use ";
        ref reason_prop;
        text " to assign a computed property.";
        text " Computed properties may only be numeric or string literal values.";
        text " See https://flow.org/en/docs/types/literals/ for more information on literal types.";
      ]
    | MessageCannotAssignToObjectWithComputedPropWithKey { reason_prop; reason_key; kind } ->
      let suffix =
        match msg_of_invalid_obj_key_kind kind with
        | Some msg -> msg
        | None ->
          [
            text " Computed properties may only be numeric or string literal values,";
            text " but this one is a ";
            ref reason_prop;
            text ". Can you add an appropriate type annotation to ";
            ref reason_key;
            text "?";
            text
              " See https://flow.org/en/docs/types/literals/ for more information on literal types.";
          ]
      in
      [text "Cannot use "; ref reason_key; text " to assign a computed property."] @ suffix
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
    | MessageCannotBuildTypedInterface sve ->
      let open Signature_error in
      let features =
        match sve with
        | ExpectedAnnotation (_, sort) ->
          [text (spf "Missing type annotation at %s:" (Expected_annotation_sort.to_string sort))]
        | UnexpectedObjectKey _ -> [text "Expected simple key in object:"]
        | UnexpectedArraySpread _ -> [text "Unexpected spread in array:"]
        | UnexpectedArrayHole _ -> [text "Unexpected array hole:"]
        | EmptyArray _ ->
          [
            text "Cannot determine the element type of an empty array. ";
            text "Please provide an annotation, e.g., by adding a type cast around this expression.";
          ]
        | EmptyObject _ ->
          [
            text "Cannot determine types of initialized properties of an empty object. ";
            text "Please provide an annotation, e.g., by adding a type cast around this expression.";
          ]
        | UnexpectedExpression (_, esort) ->
          [
            text
              (spf
                 "Cannot determine the type of this %s. "
                 (Flow_ast_utils.ExpressionSort.to_string esort)
              );
            text "Please provide an annotation, e.g., by adding a type cast around this expression.";
          ]
      in
      text "Cannot build a typed interface for this module. "
      :: text "You should annotate the exports of this module with types. "
      :: features
    | MessageCannotCallMaybeReactHook { callee_loc; hooks; non_hooks } ->
      let hook_blame =
        match hooks with
        | [] -> [text "React hook"]
        | x :: _ -> [hardcoded_string_desc_ref "React hook" x]
      in
      let non_hook_blame =
        match non_hooks with
        | [] -> [text "regular function definition"]
        | x :: _ -> [text "regular "; hardcoded_string_desc_ref "function definition" x]
      in
      [
        text "Cannot call function because ";
        hardcoded_string_desc_ref "callee" callee_loc;
        (* Kinda crummy, hopefully doesn't come up often *)
        text " may be a ";
      ]
      @ hook_blame
      @ [text " or may be a "]
      @ non_hook_blame
      @ [
          text
            ". Function callees must either be definitely a hook or definitely not a hook, because the same hook must be called every time a component renders. ";
        ]
      @ [text "(https://react.dev/reference/rules/rules-of-hooks)"]
    | MessageCannotCallNonHookSyntaxHook callee_loc ->
      [
        text "Cannot call function because ";
        hardcoded_string_desc_ref "callee" callee_loc;
        text " has a name that indicates it is a React hook (starting with ";
        code "use";
        text ") but it is not defined with hook syntax. ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageCannotCallObjectFunctionOnEnum { reason; enum_reason } ->
      let suggestion =
        match enum_name_of_reason enum_reason with
        | Some enum_name ->
          [
            text " ";
            text "You can use ";
            code (spf "%s.members()" enum_name);
            text " to get an iterator of the enum's members. ";
            text "You can turn that into an array using ";
            code "Array.from";
            text
              ", which optionally takes a second argument if you wish to also map over the result.";
          ]
        | None -> []
      in
      [
        text "Cannot call function ";
        ref reason;
        text " with argument ";
        ref enum_reason;
        text " because it is not an object.";
      ]
      @ suggestion
    | MessageCannotCallReactComponent reason ->
      [
        text "Cannot call ";
        ref reason;
        text " because React components cannot be called. Use JSX instead. ";
        text "(https://react.dev/reference/rules/react-calls-components-and-hooks)";
      ]
    | MessageCannotCallReactFunctionWithoutAtLeastNArgs { fn_name; n } ->
      [
        text "Cannot call ";
        code ("React." ^ fn_name);
        text " ";
        text
          (spf
             "without at least %d argument%s."
             n
             ( if n == 1 then
               ""
             else
               "s"
             )
          );
      ]
    | MessageCannotCallReactHookConditionally callee_loc ->
      [
        text "Cannot call ";
        hardcoded_string_desc_ref "hook" callee_loc;
        text " because React hooks cannot be called in conditional contexts. ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageCannotCallReactHookInDefinitelyNonComponentOrHook callee_loc ->
      [
        text "Cannot call ";
        hardcoded_string_desc_ref "hook" callee_loc;
        text " because React hooks can only be called within components or hooks. ";
        text "This hook is definitely not called in a component or hook. ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook callee_loc ->
      [
        text "Cannot call ";
        hardcoded_string_desc_ref "hook" callee_loc;
        text " because React hooks can only be called within components or hooks. ";
        text "Under the current configuration, ";
        text "we only infer component-syntax components to be components";
        text " and hook-syntax hooks to be hooks. ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageCannotCallReactHookInUnknownContext callee_loc ->
      [
        text "Cannot call ";
        hardcoded_string_desc_ref "hook" callee_loc;
        text " because React hooks can only be called within components or hooks. ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageCannotCallReactHookWithIllegalName callee_loc ->
      [
        text "Cannot call hook because ";
        hardcoded_string_desc_ref "callee" callee_loc;
        text "'s name does not conform to React hook rules. Hook names must begin with ";
        code "use";
        text " followed by a capitalized letter. ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageCannotCallFunctionWithExtraArg { def_reason; param_count } ->
      let msg =
        match param_count with
        | 0 -> "no arguments are expected by"
        | 1 -> "no more than 1 argument is expected by"
        | n -> spf "no more than %d arguments are expected by" n
      in
      [text msg; text " "; ref def_reason]
    | MessageCannotChangeEnumMember enum_reason ->
      [text "Cannot change member of "; ref enum_reason; text " because enums are frozen."]
    | MessageCannotCompare { lower; upper; strict_comparison_opt } ->
      [text "Cannot compare "; ref lower; text " to "; ref upper]
      @
      (match strict_comparison_opt with
      | None -> [text "."]
      | Some { left_precise_reason; right_precise_reason; strict_comparison_kind } ->
        (match strict_comparison_kind with
        | StrictComparisonGeneral ->
          [
            text ", because ";
            ref left_precise_reason;
            text " is not a subtype of ";
            ref right_precise_reason;
            text " and ";
            ref right_precise_reason;
            text " is not a subtype of ";
            ref left_precise_reason;
            text
              ". In **rare** cases, these types may have overlapping values but lack a subtyping relationship. ";
            text
              "If that happens, you can cast one side to the union of both types to pass the flow check. ";
          ]
        | StrictComparisonNull { null_side } ->
          let (null_side_reason, the_other_side_reason) =
            match null_side with
            | `Left -> (left_precise_reason, right_precise_reason)
            | `Right -> (right_precise_reason, left_precise_reason)
          in
          [
            text ", because";
            no_desc_ref (loc_of_reason null_side_reason);
            text " is null and ";
            ref the_other_side_reason;
            text " does not contain null. ";
            text "Perhaps you meant to use ";
            code "==";
            text ", which checks for both ";
            code "undefined";
            text " and ";
            code "null";
            text "?";
          ]
        | StrictComparisonEmpty { empty_side } ->
          [
            text ", because ";
            (match empty_side with
            | `Left -> ref left_precise_reason
            | `Right -> ref right_precise_reason);
            text " is empty. ";
          ]))
    | MessageCannotCompareNonStrict { lower; upper } ->
      [
        text "Cannot compare ";
        ref lower;
        text " to ";
        ref upper;
        text " with a non-strict equality check. ";
        text "Make sure the arguments are valid, ";
        text "or try using strict equality (";
        code "===";
        text " or ";
        code "!==";
        text ") instead.";
      ]
    | MessageCannotCreateExactType lower ->
      [text "Cannot create exact type from "; ref lower; text "."]
    | MessageCannotDeclareAlreadyBoundGlobal x ->
      [
        text "Cannot redeclare global ";
        Friendly.ref x;
        text " because the global is already declared in another file.";
      ]
    | MessageCannotDeclareAlreadyBoundName x ->
      [text "Cannot declare "; Friendly.ref x; text " because the name is already bound."]
    | MessageCannotDeclareAlreadyBoundNameInCoreJs x ->
      [text "Cannot declare "; Friendly.ref x; text " because the name is a core builtin type."]
    | MessageCannotDeclareAlreadyBoundNameInNamespace x ->
      [
        text "Cannot declare the name in the namespace because the name ";
        ref x;
        text " is already bound.";
      ]
    | MessageCannotDeclareReservedType { reason; keyword } ->
      [
        text "Cannot declare ";
        Friendly.ref reason;
        text ", because ";
        code (IncorrectType.incorrect_of_kind keyword);
        text " is a reserved type name.";
      ]
    | MessageCannotDelete expr ->
      [
        text "Cannot delete ";
        ref expr;
        text " because only member expressions and variables can be deleted.";
      ]
    | MessageCannotDetermineEmptyArrayLiteralType ->
      [text "Cannot determine type of empty array literal. Please provide an annotation."]
    | MessageCannotDetermineModuleType ->
      [
        text "Unable to determine module type (CommonJS vs ES) if both an export ";
        text "statement and ";
        code "module.exports";
        text " are used in the ";
        text "same module!";
      ]
    | MessageCannotExportRenamedDefault { name; is_reexport } ->
      let reexport_message () =
        [
          text "If you intended to set the default export please ";
          code "import";
          text " and then ";
          code "export default";
          text " instead.";
        ]
      in
      (match (name, is_reexport) with
      | (None, _) ->
        [
          text "Cannot set the default export of a module by re-exporting the ";
          code "default";
          text " property. ";
        ]
        @ reexport_message ()
      | (Some name, true) ->
        [
          text "Cannot set the default export of a module by re-exporting ";
          code name;
          text " as ";
          code "default";
          text ". ";
        ]
        @ reexport_message ()
      | (Some name, false) ->
        [
          text "Cannot set the default export of a module by renaming ";
          code name;
          text " to ";
          code "default";
          text ". If you intended to set the default export use ";
          code (spf "export default %s" name);
          text " instead.";
        ])
    | MessageCannotExhaustivelyCheckAbstractEnums { description; enum_reason } ->
      [
        text "Cannot exhaustively check ";
        desc description;
        text " because ";
        ref enum_reason;
        text " is an abstract enum value, so has no members.";
      ]
    | MessageCannotExhaustivelyCheckEnumWithUnknowns { description; enum_reason } ->
      [
        text "Missing ";
        code "default";
        text " case in the check of ";
        desc description;
        text ". ";
        ref enum_reason;
        text " has unknown members (specified using ";
        code "...";
        text ") so checking it requires the use of a ";
        code "default";
        text " case to cover those members.";
      ]
    | MessageCannotImplementNonInterface i ->
      [text "Cannot implement "; desc i; text " because it is not an interface."]
    | MessageCannotInstantiateObjectUtilTypeWithEnum { description; enum_reason } ->
      let suggestion =
        match enum_name_of_reason enum_reason with
        | Some enum_name ->
          [
            text " ";
            text "You can use the enum's name ";
            code enum_name;
            text " as the type of its members.";
          ]
        | None -> []
      in
      [
        text "Cannot instantiate ";
        desc description;
        text " because ";
        ref enum_reason;
        text " is not an object.";
      ]
      @ suggestion
    | MessageCannotIterateEnum { reason; for_in = true } ->
      let suggestion =
        match enum_name_of_reason reason with
        | Some enum_name ->
          [
            text " ";
            text "You can use ";
            code (spf "for (... of %s.members()) { ... }" enum_name);
            text " to iterate over the enum's members.";
          ]
        | None -> []
      in
      [
        text "Cannot iterate using a ";
        code "for...in";
        text " loop because ";
        ref reason;
        text " is not an object, null, or undefined.";
      ]
      @ suggestion
    | MessageCannotIterateEnum { reason; for_in = false } ->
      let suggestion =
        match enum_name_of_reason reason with
        | Some enum_name ->
          [
            text " ";
            text "You can use ";
            code (spf "%s.members()" enum_name);
            text " to get an iterator for the enum's members.";
          ]
        | None -> []
      in
      [desc (desc_of_reason reason); text " is not an iterable."] @ suggestion
    | MessageCannotIterateWithForIn reason ->
      [
        text "Cannot iterate using a ";
        code "for...in";
        text " statement ";
        text "because ";
        ref reason;
        text " is not an object, null, or undefined.";
      ]
    | MessageCannotMutateThisPrototype -> [text "Mutating this prototype is unsupported."]
    | MessageCannotNestComponents ->
      [text "Components may not be nested directly within other components or hooks."]
    | MessageCannotNestHook ->
      [text "Hooks may not be nested directly within other components or hooks."]
    | MessageCannotOptimizeUnionDueToNonUniqueKeys non_unique_keys ->
      let string_of_union_enum x = code (Type.UnionEnum.to_string x) in
      let string_of_non_unique_key (name, map) =
        let ref_texts (r0, rs) = ref r0 :: List.concat_map (fun r -> [text ", "; ref r]) rs in
        map
        |> Type.UnionRep.UnionEnumMap.bindings
        |> List.concat_map (fun (enum, rs) ->
               [
                 text "\n - Key ";
                 code (display_string_of_name name);
                 text " has value ";
                 string_of_union_enum enum;
                 text " in ";
               ]
               @ ref_texts rs
               @ [text "."]
           )
      in
      let keys =
        non_unique_keys
        |> NameUtils.Map.bindings
        |> Base.List.concat_map ~f:string_of_non_unique_key
      in
      text
        "Union could not be fully optimized internally. The following keys have non-unique values:"
      :: keys
    | MessageCannotOptimizeUnionInternally kind ->
      let kind =
        let open Type.UnionRep in
        match kind with
        | ContainsUnresolved r ->
          [
            text "The form of ";
            ref r;
            text " is not supported for optimization. ";
            text "Try replacing this type with a simpler alternative.";
          ]
        | NoCandidateMembers ->
          [
            text "The union needs to include in its members ";
            text "at least one of: ";
            text "object type, string literal, numeric literal, ";
            text "boolean literal, void or null types.";
          ]
        | NoCommonKeys -> [text "There are no common keys among the members of the union."]
      in
      text "Union could not be optimized internally. " :: kind
    | MessageCannotPassReactRefAsArgument { usage; in_hook } ->
      let context =
        if in_hook then
          text "within hooks"
        else
          text "during render"
      in
      [
        text "Cannot pass ";
        ref usage;
        text " as an argument because ";
        code "ref";
        text " values may not be passed to functions because they could read the ref value (";
        code "current";
        text ") property) ";
        context;
        text ". (https://react.dev/reference/react/useRef).";
      ]
    | MessageCannotPerformArithOnNonNumbersOrBigInt reason ->
      [
        text "Cannot perform arithmetic operation because ";
        ref reason;
        text " ";
        text "is not a number or bigint.";
      ]
    | MessageCannotPerformBigIntRShift3 reason ->
      [
        text "Cannot perform unsigned right shift because ";
        ref reason;
        text " ";
        text "is a bigint, and all bigints are signed.";
      ]
    | MessageCannotPerformBigIntUnaryPlus reason ->
      [
        text "Cannot perform unary plus because a ";
        ref reason;
        text " ";
        text "cannot be coerced to number.";
      ]
    | MessageCannotPerformBinaryArith { kind; reason_l; reason_r } ->
      [
        text "Cannot use operator `";
        text (Type.ArithKind.string_of_arith_kind kind);
        text "` with operands ";
        ref reason_l;
        text " and ";
        ref reason_r;
      ]
    | MessageCannotReassignConstant x ->
      [text "Cannot reassign constant "; Friendly.ref x; text "."]
    | MessageCannotReassignConstantLikeBinding { definition; binding_kind } ->
      [
        text "Cannot reassign ";
        text (string_of_assigned_const_like_binding_type binding_kind);
        text " binding ";
        ref definition;
        text ".";
      ]
    | MessageCannotReassignEnum x -> [text "Cannot reassign enum "; Friendly.ref x; text "."]
    | MessageCannotReassignImport x -> [text "Cannot reassign import "; Friendly.ref x; text "."]
    | MessageCannotRedeclareVar x ->
      [text "Cannot declare "; Friendly.ref x; text " because var redeclaration is not supported."]
    | MessageCannotReferenceTypeGuardParameter { type_guard_reason; binding_reason } ->
      [text "A "; ref type_guard_reason; text " cannot reference "; ref binding_reason; text "."]
    | MessageCannotResolveBuiltinName name -> [text "Cannot resolve name "; code name; text "."]
    | MessageCannotResolveBuiltinModule { name; potential_generator } ->
      let potential_generator_features =
        match potential_generator with
        | Some generator ->
          [
            text " Try running the command "; code generator; text " to generate the missing module.";
          ]
        | None -> []
      in
      [text "Cannot resolve module "; code name; text "."] @ potential_generator_features
    | MessageCannotResolveExpectedModule { name; expected_module_purpose } ->
      let explanation =
        match expected_module_purpose with
        | ReactModuleForJSXFragment ->
          [text "The "; code "react"; text " module must exist to type check JSX fragments."]
        | ReactModuleForReactClassComponent ->
          [
            text "The ";
            code "react";
            text " module must exist to type check React class components.";
          ]
        | ReactModuleForReactMixedElementType ->
          [
            text "The ";
            code "react";
            text " module must exist to provide a type for ";
            code "React.MixedElement";
            text ".";
          ]
        | ReactModuleForReactNodeType ->
          [
            text "The ";
            code "react";
            text " module must exist to provide a type for ";
            code "React.Node";
            text ".";
          ]
        | ReactModuleForReactRefSetterType ->
          [
            text "The ";
            code "react";
            text " module must exist to provide a type for ";
            code "React.RefSetter";
            text ".";
          ]
        | ReactModuleForReactElementRefType ->
          [
            text "The ";
            code "react";
            text " module must exist to provide a type for ";
            code "React.ElementRef";
            text ".";
          ]
      in
      [text "Cannot resolve module "; code name; text ". "] @ explanation
    | MessageCannotSpreadDueToPotentialOverwrite { spread_reason; object_reason; key_reason } ->
      [
        text "Flow cannot determine a type for ";
        ref spread_reason;
        text ". ";
        ref object_reason;
        text " cannot be spread because the indexer ";
        ref key_reason;
        text " may overwrite properties with explicit keys in a way that Flow cannot track. ";
        text "Try spreading ";
        ref object_reason;
        text " first or remove the indexer";
      ]
    | MessageCannotSpreadGeneral
        { spread_reason; object1_reason; object2_reason; propname; error_kind } ->
      let (error_reason, fix_suggestion) =
        match error_kind with
        | UnexpectedInexact ->
          ("is inexact", [text " Try making "; ref object2_reason; text " exact"])
        | UnexpectedIndexer ->
          ( "has an indexer",
            [
              text " Try removing the indexer in ";
              ref object2_reason;
              text " or make ";
              code (display_string_of_name propname);
              text " a required property";
            ]
          )
      in
      [
        text "Flow cannot determine a type for ";
        ref spread_reason;
        text ". ";
        ref object2_reason;
        text " ";
        text error_reason;
        text ", so it may contain ";
        code (display_string_of_name propname);
        text " with a type that conflicts with ";
        code (display_string_of_name propname);
        text "'s definition in ";
        ref object1_reason;
        text ".";
      ]
      @ fix_suggestion
    | MessageCannotSpreadInexactMayOverwriteIndexer
        { spread_reason; key_reason; value_reason; object2_reason } ->
      [
        text "Flow cannot determine a type for ";
        ref spread_reason;
        text ". ";
        ref object2_reason;
        text " is inexact and may ";
        text "have a property key that conflicts with ";
        ref key_reason;
        text " or a property value that conflicts with ";
        ref value_reason;
        text ". Try making ";
        ref object2_reason;
        text " exact";
      ]
    | MessageCannotSpreadInterface { spread_reason; interface_reason } ->
      [
        text "Flow cannot determine a type for ";
        ref spread_reason;
        text ". ";
        ref interface_reason;
        text " cannot be spread because interfaces do not ";
        text "track the own-ness of their properties. Try using an object type instead";
      ]
    | MessageCannotUseAsConstructor reason ->
      [
        text "Cannot use ";
        code "new";
        text " on ";
        ref reason;
        text ". Only classes can be constructed.";
      ]
    | MessageCannotUseAsPrototype reason ->
      [text "Cannot use "; ref reason; text " as a prototype. Expected an object or null."]
    | MessageCannotUseAsSuperClass reason ->
      [
        text "Cannot use ";
        ref reason;
        text " as a superclass. Only variables and member expressions may be extended";
      ]
    | MessageCannotUseBeforeDeclaration x ->
      [
        text "Cannot use variable ";
        Friendly.ref x;
        text " because the declaration ";
        text "either comes later or was skipped.";
      ]
    | MessageCannotUseThisSuperBeforeSuperCall x ->
      [
        text "Must call ";
        code "super";
        text " before accessing ";
        Friendly.ref x;
        text " in a derived constructor.";
      ]
    | MessageCannotUseComputedPropertyWithUnion computed_property_reason ->
      [
        text "Cannot use ";
        ref computed_property_reason;
        text " as a computed property.";
        text " Computed properties may only be primitive literal values, but the type of ";
        ref computed_property_reason;
        text " is a union. Can you add a literal type annotation to ";
        ref computed_property_reason;
        text "?";
        text " See https://flow.org/en/docs/types/literals/ for more information on literal types.";
      ]
    | MessageCannotUseDefaultImportWithDestrucuturing ->
      [
        text "The default export of a module cannot be accessed from import destructuring. ";
        text "To use the default export you must import it directly.";
      ]
    | MessageCannotUseDollarExports ->
      [
        text "Cannot use ";
        code "$Exports";
        text " because the first type ";
        text "argument must be a string literal.";
      ]
    | MessageCannotUseEnumMemberUsedAsType { description; enum_reason } ->
      [
        text "Cannot use ";
        desc description;
        text " as a type. ";
        text "Enum members are not separate types. ";
        text "Only the enum itself, ";
        ref enum_reason;
        text ", is a type.";
      ]
    | MessageCannotUseExportInNonLegalToplevelContext name ->
      [code name; text " may only be used as part of a legal top level export statement"]
    | MessageCannotUseImportStar import_star_reason ->
      [
        ref import_star_reason;
        text " object can only be used by accessing one of its named exports";
        text " with a member access or destructuring.";
      ]
    | MessageCannotUseInOperatorDueToBadLHS reason ->
      (* TODO: or symbol *)
      [
        text "Cannot use ";
        code "in";
        text " because on the left-hand side, ";
        ref reason;
        text " must be a string or number.";
      ]
    | MessageCannotUseInOperatorDueToBadRHS reason ->
      [
        text "Cannot use ";
        code "in";
        text " because on the right-hand side, ";
        ref reason;
        text " must be an object or array.";
      ]
    | MessageCannotUseInstanceOfOperatorDueToBadRHS reason ->
      [
        text "The right-hand side of an ";
        code "instanceof";
        text " expression must be an object, but got ";
        ref reason;
        text ".";
      ]
    | MessageCannotUseMixedImportAndRequire import_reason ->
      [
        text "Cannot use a mix of non-type toplevel ";
        ref import_reason;
        text " and ";
        code "require";
        text " statements in the same file.";
      ]
    | MessageCannotUseNonPolymorphicTypeWithTypeArgs { is_new; reason_arity; expected_arity = n } ->
      let use =
        if is_new then
          "construct "
        else
          "call "
      in
      if n = 0 then
        [
          text "Cannot ";
          text use;
          text "non-polymorphic ";
          ref reason_arity;
          text " with type arguments.";
        ]
      else
        [
          text "Cannot ";
          text use;
          ref reason_arity;
          text " without exactly ";
          text
            (spf
               "%n type argument%s."
               n
               ( if n == 1 then
                 ""
               else
                 "s"
               )
            );
        ]
    | MessageCannotUsePrimitiveAsInterface { reason; interface_reason; kind } ->
      let kind_str =
        match kind with
        | `Boolean -> "Boolean"
        | `Number -> "Number"
        | `String -> "String"
      in
      [
        ref reason;
        text ", a primitive, cannot be used as a subtype of ";
        ref interface_reason;
        text ". ";
        text "You can wrap it in ";
        code (spf "new %s(...))" kind_str);
        text " to turn it into an object and attempt to use it as a subtype of an interface";
      ]
    | MessageCannotUseStrUtilType ->
      [
        text "Cannot use ";
        code "StringPrefix";
        text " because the first type argument must be a string literal.";
      ]
    | MessageCannotUseTypeDueToPolarityMismatch { reason_targ; expected_polarity; actual_polarity }
      ->
      let polarity_string = function
        | Polarity.Positive -> "output"
        | Polarity.Negative -> "input"
        | Polarity.Neutral -> "input/output"
      in
      let expected_polarity = polarity_string expected_polarity in
      let actual_polarity = polarity_string actual_polarity in
      [
        text "Cannot use ";
        ref reason_targ;
        text (" in an " ^ actual_polarity ^ " ");
        text "position because ";
        ref reason_targ;
        text " is expected to occur only in ";
        text (expected_polarity ^ " positions.");
      ]
    | MessageCannotUseTypeForAnnotationInference { reason_op; reason; suggestion } ->
      let suggestion =
        match suggestion with
        | Some util -> [text " (Try using the "; code util; text " utility type instead.)"]
        | None -> []
      in
      [
        text "Cannot use ";
        desc (desc_of_reason reason_op);
        text " on ";
        ref (replace_desc_reason (Reason.desc_of_reason reason) reason);
        text " in an export position. ";
        text "Please provide an (alternative) annotation for ";
        ref reason_op;
        text ".";
      ]
      @ suggestion
    | MessageCannotUseTypeGuardWithFunctionParamHavoced { type_guard_desc; param_reason; call_locs }
      ->
      let loc_str =
        match call_locs with
        | [] -> [text "in this function"]
        | [loc] -> [text "in"; no_desc_ref loc]
        | _ -> text "in the following expressions:" :: Base.List.map call_locs ~f:no_desc_ref
      in
      [
        text "Cannot use ";
        desc type_guard_desc;
        text ", because ";
        ref param_reason;
        text " is reassigned ";
      ]
      @ loc_str
      @ [text "."]
    | MessageCannotUseTypeInValuePosition { reason; type_only_namespace; imported_name } ->
      let base =
        let ref_x = Friendly.ref reason in
        if type_only_namespace then
          [
            text "Cannot use type-only namespace ";
            ref_x;
            text " as a value. ";
            text "Type-only namespaces are erased and don't exist at runtime.";
          ]
        else
          [
            text "Cannot use type ";
            ref_x;
            text " as a value. ";
            text "Types are erased and don't exist at runtime.";
          ]
      in
      (match imported_name with
      | None -> base
      | Some name ->
        base
        @ [
            text " If the exported binding can also be used as a value, try importing it using ";
            code (spf "import %s" name);
            text " instead of ";
            code (spf "import type %s" name);
            text " and ";
            code (spf "import {%s}" name);
            text " instead of ";
            code (spf "import type {%s}" name);
            text ".";
          ])
    | MessageCannotUseTypeWithInvalidTypeArgs { reason_main; reason_tapp } ->
      [text "Cannot use "; ref reason_main; text " with "; ref reason_tapp; text " argument"]
    | MessageCannotUseTypeWithoutAnyTypeArgs { reason_arity; min_arity; max_arity } ->
      let (arity, args) =
        if min_arity = max_arity then
          ( spf "%d" max_arity,
            if max_arity = 1 then
              "argument"
            else
              "arguments"
          )
        else
          (spf "%d-%d" min_arity max_arity, "arguments")
      in
      [text "Cannot use "; ref reason_arity; text (spf " without %s type %s." arity args)]
    | MessageCannotUseTypeWithoutAtLeastNTypeArgs n ->
      [
        text "Cannot use type without at least ";
        text
          (spf
             "%n type %s."
             n
             ( if n == 1 then
               "argument"
             else
               "arguments"
             )
          );
      ]
    | MessageCannotUseTypeWithoutExactlyNTypeArgs n ->
      [
        text "Cannot use type without exactly ";
        text
          (spf
             "%n type %s."
             n
             ( if n == 1 then
               "argument"
             else
               "arguments"
             )
          );
      ]
    | MessageCannotUseTypeWithTooFewTypeArgs { reason_arity; n } ->
      [
        text "Cannot use ";
        ref reason_arity;
        text " with fewer than ";
        text
          (spf
             "%n type %s."
             n
             ( if n == 1 then
               "argument"
             else
               "arguments"
             )
          );
      ]
    | MessageCannotUseTypeWithTooManyTypeArgs { reason_arity; n } ->
      [
        text "Cannot use ";
        ref reason_arity;
        text " with more than ";
        text
          (spf
             "%n type %s."
             n
             ( if n == 1 then
               "argument"
             else
               "arguments"
             )
          );
      ]
    | MessageComponentMissingReturn reason ->
      [
        text "Cannot declare component because ";
        ref reason;
        text
          " is not guaranteed to reach a return statement. An explicit return statement must be included for all possible branches.";
      ]
    | MessageComponentMissingBody -> [text "Components in non-ambient contexts must have a body."]
    | MessageComponentBodyInAmbientContext ->
      [
        text
          "Components in ambient contexts (library definitions, .flow files, declare module, declare namespace) cannot have a body.";
      ]
    | MessageComponentNonUpperCase ->
      [text "Component identifiers must begin with an upper-case character"]
    | MessageDeclareComponentInvalidParam kind ->
      (match kind with
      | DeclareComponentParamAsBinding ->
        [
          text
            "Cannot use `as` binding in `declare component` parameters, unless the param name is a string literal. Use the type-only syntax `paramName: Type` instead.";
        ]
      | DeclareComponentParamDefaultValue ->
        [text "Cannot use default values in `declare component` parameters."]
      | DeclareComponentParamMissingAnnotation ->
        [text "Missing type annotation on `declare component` parameter."]
      | DeclareComponentParamStringLiteralWithoutAs ->
        [
          text
            "String literal parameter names in `declare component` require an `as` binding. Use `'param-name' as paramName: Type`.";
        ])
    | MessageDefinitionCycle dependencies ->
      let compare a b = Loc.compare (loc_of_aloc a) (loc_of_aloc b) in
      let deps =
        Base.List.filter_mapi
          ~f:
            (fun i -> function
              | (_, [], _) -> None
              | _ when i = 10 -> Some [text " - ...\n"]
              | _ when i > 10 -> None
              | (reason, (_ :: _ as dep), _) ->
                let (hd, tl) = Base.List.dedup_and_sort ~compare dep |> Nel.of_list_exn in
                let (suffix, tl) =
                  if List.length tl > 4 then
                    ([text ", [...]"], Base.List.take tl 4)
                  else
                    ([], tl)
                in
                let tl_dep =
                  Base.List.map ~f:(fun loc -> [text ","; no_desc_ref loc]) tl |> List.flatten
                in
                Some
                  ([
                     text " - ";
                     ref reason;
                     text " depends on ";
                     hardcoded_string_desc_ref "other definition" hd;
                   ]
                  @ tl_dep
                  @ suffix
                  @ [text "\n"]
                  ))
          (Nel.to_list dependencies)
        |> List.flatten
      in
      let (locs, properties) =
        Base.List.fold
          ~init:([], [])
          ~f:(fun (locs, properties) (_, _, annot_locs) ->
            Base.List.fold
              annot_locs
              ~init:(locs, properties)
              ~f:(fun (locs, properties) annot_loc ->
                match annot_loc with
                | Env_api.Loc l -> (l :: locs, properties)
                | Env_api.Object { loc; props } -> (loc :: locs, props @ properties)
            ))
          (Nel.to_list dependencies)
      in
      let locs = Base.List.take (Base.List.dedup_and_sort ~compare locs) 10 in
      let properties = Base.List.dedup_and_sort ~compare properties in
      let annot_message ls = Base.List.map ~f:no_desc_ref ls in
      let features =
        text
          "The following definitions recursively depend on each other, and Flow cannot compute their types:\n"
        :: deps
        @ (text "Please add type annotations to these definitions" :: annot_message locs)
      in
      if List.length properties <= 5 && List.length properties > 0 then
        features @ (text " or to these object properties" :: annot_message properties)
      else
        features
    | MessageDefinitionInvalidRecursive { description; recursion; annot_locs } ->
      let (itself, tl_recur) =
        match recursion with
        | hd :: tl ->
          let (suffix, tl) =
            if List.length tl > 4 then
              ([text ", [...]"], Base.List.take tl 4)
            else
              ([], tl)
          in
          ( hardcoded_string_desc_ref "itself" hd,
            (Base.List.map ~f:(fun loc -> [text ", "; no_desc_ref loc]) tl |> List.flatten) @ suffix
          )
        | [] -> (text "itself", [])
      in
      let annot_message =
        match annot_locs with
        | [] -> [text "this definition"]
        | [Env_api.Loc loc]
        | [Env_api.Object { loc; props = [] }] ->
          [hardcoded_string_desc_ref "this definition" loc]
        | [Env_api.Object { loc; props }] when List.length props > 5 ->
          [hardcoded_string_desc_ref "this definition" loc]
        | [Env_api.Object { loc; props = [prop] }] ->
          [
            hardcoded_string_desc_ref "this definition" loc;
            text "or to";
            hardcoded_string_desc_ref "its property" prop;
          ]
        | [Env_api.Object { loc; props }] ->
          [hardcoded_string_desc_ref "this definition" loc; text " or to its properties"]
          @ Base.List.map ~f:no_desc_ref props
        | ls ->
          let (locs, properties) =
            Base.List.fold
              ~init:([], [])
              ~f:(fun (locs, properties) annot_locs ->
                match annot_locs with
                | Env_api.Loc l -> (l :: locs, properties)
                | Env_api.Object { loc; props } -> (loc :: locs, props @ properties))
              ls
          in
          let compare a b = Loc.compare (loc_of_aloc a) (loc_of_aloc b) in
          let locs = Base.List.take (Base.List.dedup_and_sort ~compare locs) 10 in
          let properties = Base.List.dedup_and_sort ~compare properties in
          let props =
            if List.length properties <= 5 && List.length properties > 0 then
              let these =
                if List.length properties > 1 then
                  text "these object properties"
                else
                  text "this object property"
              in
              text " or to " :: these :: Base.List.map ~f:no_desc_ref properties
            else
              []
          in
          (text "these definitions" :: Base.List.map ~f:no_desc_ref locs) @ props
      in
      [
        text "Cannot compute a type for ";
        desc description;
        text " because its definition includes references to ";
        itself;
      ]
      @ tl_recur
      @ (text ". Please add an annotation to " :: annot_message)
    | MessageDeprecatedBool -> [text "Deprecated type. Use "; code "boolean"; text " instead."]
    | MessageDevOnlyRefinedLocInfo { refining_locs } ->
      text "Refined at" :: Base.List.map refining_locs ~f:no_desc_ref
    | MessageDevOnlyInvalidatedRefinementInfo invalidation_info ->
      let invalidation_info_msg =
        Base.List.map invalidation_info ~f:(fun (loc, reason) ->
            let reason = Refinement_invalidation.string_of_reason reason in
            [text reason; text " at"; no_desc_ref loc]
        )
        |> Flow_errors_utils.Friendly.conjunction_concat
      in
      text "Refinement invalidated due to " :: invalidation_info_msg
    | MessageDocblockError err ->
      (match err with
      | MultipleFlowAttributes ->
        [
          text "Unexpected ";
          code "@flow";
          text " declaration. Only one per ";
          text "file is allowed.";
        ]
      | InvalidFlowMode s ->
        [
          code (spf "@flow %s" s);
          text " is not a valid ";
          code "@flow";
          text " mode. Valid ones are: ";
          code "@flow";
          text ", ";
          code "@flow strict";
          text ", and ";
          code "@flow strict-local";
          text ".";
        ]
      | MultipleJSXAttributes ->
        [
          text "Unexpected ";
          code "@jsx";
          text " declaration. Only one per ";
          text "file is allowed.";
        ]
      | InvalidJSXAttribute first_error ->
        [
          text "Invalid ";
          code "@jsx";
          text " declaration. Should have the form ";
          code "@jsx LeftHandSideExpression";
          text " with no spaces.";
        ]
        @
        (match first_error with
        | None -> []
        | Some first_error -> [text (spf " Parse error: %s." first_error)])
      | MultipleJSXRuntimeAttributes ->
        [
          text "Unexpected ";
          code "@jsxRuntime";
          text " declaration. Only one per ";
          text "file is allowed.";
        ]
      | InvalidJSXRuntimeAttribute ->
        [
          text "Invalid ";
          code "@jsxRuntime";
          text " declaration. The only supported values are ";
          code "classic";
          text " and ";
          code "automatic";
          text ".";
        ]
      | InvalidSupportsPlatform p ->
        [
          text "Invalid ";
          code "@supportsPlatform";
          text " declaration. ";
          code p;
          text " is not configured in ";
          code "experimental.multi_platform.extensions";
          text " in your flow config.";
        ]
      | DisallowedSupportsPlatform ->
        [code "@supportsPlatform"; text " declaration is disallowed in platform specific files."])
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
    | MessageDuplicateClassMember { name; static; class_kind } ->
      let static =
        if static then
          "static "
        else
          ""
      in
      let (class_kind, item_name) =
        match class_kind with
        | ClassKind.Class -> ("class", "member")
        | ClassKind.Record -> ("record", "property")
      in
      let member_type = String.capitalize_ascii (spf "%s%s %s" static class_kind item_name) in
      [
        code name;
        text " has already been declared in this ";
        text class_kind;
        text ". ";
        text member_type;
        text " names must be unique.";
      ]
    | MessageDuplicateEnumMember { prev_use_loc; enum_reason } ->
      [
        text "Invalid enum member initializer. Initializers need to be unique, but this one ";
        text "has already been used for a ";
        hardcoded_string_desc_ref "previous member" prev_use_loc;
        text " of ";
        ref enum_reason;
        text ".";
      ]
    | MessageDuplicateModuleProvider { module_name; provider; conflict } ->
      let file_of_loc l = l |> loc_of_aloc |> Loc.source in
      (match (file_of_loc provider, file_of_loc conflict) with
      | (Some provider_file, Some conflict_file)
        when File_key.check_suffix provider_file Files.flow_ext
             && File_key.check_suffix conflict_file ".js" ->
        [
          text "This file is being illegally shadowed by the ";
          hardcoded_string_desc_ref "js.flow file" provider;
          text ". This file can only be shadowed by a js.flow file ";
          text "in the same directory with the same base name.";
        ]
      | _ ->
        [
          text "Duplicate module provider for ";
          code module_name;
          text ". Change ";
          text "either the name of this file or the ";
          hardcoded_string_desc_ref "name of the current module provider" provider;
          text ".";
        ])
    | MessageEnumsNotEnabled ->
      [
        text "Flow Enums are not enabled. ";
        text "You may opt-in to using enums by putting ";
        code "enums=true";
        text " into the ";
        code "[options]";
        text " section of your ";
        code ".flowconfig";
        text ".";
      ]
    | MessageEnumConstNotSupported ->
      [
        code "const";
        text
          " enums are not supported. Flow Enums are designed to allow for inlining, however the inlining itself needs to be part of the build system (whatever you use) rather than Flow itself.";
      ]
    | MessageInvalidEnumMemberName { member_name; enum_reason } ->
      let suggestion = String.capitalize_ascii member_name in
      [
        text "Enum member names cannot start with lowercase 'a' through 'z'. Instead of using ";
        code member_name;
        text ", consider using ";
        code suggestion;
        text ", in ";
        ref enum_reason;
        text ".";
      ]
    | MessageExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 } ->
      let format_reason_group { first_reason; second_reason } =
        match second_reason with
        | None -> [ref first_reason]
        | Some second_reason ->
          [text "inferred union from "; ref first_reason; text " | "; ref second_reason]
      in
      let union_refs =
        let reasons_for_operand1 = format_reason_group reasons_for_operand1 in
        let reasons_for_operand2 = format_reason_group reasons_for_operand2 in
        reasons_for_operand1 @ [text " and "] @ reasons_for_operand2
      in
      [
        text "Computing ";
        ref reason;
        text " may lead to an exponentially large number of cases to reason about because ";
      ]
      @ union_refs
      @ [
          text
            " are both unions. Please use at most one union type per spread to simplify reasoning about the spread result.";
          text
            " You may be able to get rid of a union by specifying a more general type that captures all of the branches of the union.";
        ]
    | MessageExportValueAsType name ->
      [text "Cannot export the value "; code name; text " as a type."]
    | MessageFunctionRequiresAnotherArgument { def; from = None } ->
      [ref def; text " requires another argument"]
    | MessageFunctionRequiresAnotherArgument { def; from = Some from } ->
      [ref def; text " requires another argument from "; ref from]
    | MessageImplicitInexactObject ->
      [
        text "Please add ";
        code "...";
        text " to the end of the list of ";
        text "properties to express an inexact object type.";
      ]
    | MessageImportTypeAsTypeof export_name ->
      let (prefix, export) = msg_export "the type " export_name in
      [
        text "Cannot import ";
        prefix;
        export;
        text " as a type. ";
        code "import typeof";
        text " only works on value exports like variables, ";
        text "functions, and classes. If you intended to import a type use ";
        code "import type";
        text " instead.";
      ]
    | MessageImportTypeAsValue export_name ->
      let (prefix, export) = msg_export "the type " export_name in
      [
        text "Cannot import ";
        prefix;
        export;
        text " as a value. ";
        text "Use ";
        code "import type";
        text " instead.";
      ]
    | MessageImportValueAsType export_name ->
      let (prefix, export) = msg_export "the value " export_name in
      [
        text "Cannot import ";
        prefix;
        export;
        text " as a type. ";
        code "import type";
        text " only works on type exports like type aliases, ";
        text "interfaces, and classes. If you intended to import the type of a ";
        text "value use ";
        code "import typeof";
        text " instead.";
      ]
    | MessageIncompatibleArity { lower; lower_arity; upper; upper_arity } ->
      [
        text "arity ";
        text (string_of_int lower_arity);
        text " of ";
        ref lower;
        text " is incompatible with arity ";
        text (string_of_int upper_arity);
        text " of ";
        ref upper;
      ]
    | MessageIncompatibleTupleArity
        {
          lower_reason;
          lower_arity;
          lower_inexact;
          upper_reason;
          upper_arity;
          upper_inexact;
          unify;
        } ->
      let str_of_arity ~inexact (num_req, num_total) =
        let suffix =
          if inexact then
            "or more elements (inexact tuple)"
          else
            "elements"
        in
        if num_req = num_total then
          if num_total = 1 && not inexact then
            spf "%d element" num_total
          else
            spf "%d %s" num_total suffix
        else
          spf "%d-%d %s" num_req num_total suffix
      in
      if unify && upper_inexact && not upper_inexact then
        [
          ref upper_reason;
          text " and ";
          ref lower_reason;
          text " do not have the same amount of elements. ";
          ref upper_reason;
          text " is inexact, but ";
          ref lower_reason;
          text " is not.";
        ]
      else
        [
          ref lower_reason;
          text " has ";
          text (str_of_arity ~inexact:lower_inexact lower_arity);
          text " but ";
          ref upper_reason;
          text " has ";
          text (str_of_arity ~inexact:upper_inexact upper_arity);
        ]
    | MessageIncompatibleImplicitReturn { lower; upper } ->
      [ref lower; text " is incompatible with "; text "implicitly-returned "; desc upper]
    | MessageIncompatibleClassToObject { reason_class; reason_obj; kind } ->
      let kind =
        match kind with
        | Flow_intermediate_error_types.ClassKind.Class -> "Class instances"
        | Flow_intermediate_error_types.ClassKind.Record -> "Records"
      in
      [
        ref reason_class;
        text " is not a subtype of ";
        ref reason_obj;
        text ". ";
        text kind;
        text " are not subtypes of object types; consider rewriting ";
        ref reason_obj;
        text " as an interface";
      ]
    | MessageIncompatibleComponentRestParam rest_param ->
      [
        text "Cannot use ";
        ref rest_param;
        text
          " as a component rest param. Component rest params must use an object type and cannot be optional";
      ]
    | MessageIncompatibleGeneral { lower; upper } ->
      [ref lower; text " is incompatible with "; ref upper]
    | MessageIncompatibleGeneralWithPrintedTypes { lower_loc; upper_loc; lower_desc; upper_desc } ->
      [
        ref_of_ty_or_desc lower_loc lower_desc;
        text " is incompatible with ";
        ref_of_ty_or_desc upper_loc upper_desc;
      ]
    | MessageIncompatibleDueToInvariantSubtyping
        { sub_component; lower_loc; upper_loc; lower_desc; upper_desc } ->
      (match sub_component with
      | None ->
        [
          ref_of_ty_or_desc lower_loc lower_desc;
          text " is not exactly the same as ";
          ref_of_ty_or_desc upper_loc upper_desc;
        ]
      | Some (SubComponentOfInvariantSubtypingError.ObjectProps props) ->
        text "properties "
        :: (Base.List.map props ~f:(fun prop -> [code (display_string_of_name prop)])
           |> Flow_errors_utils.Friendly.conjunction_concat
           )
        @ [
            text " of ";
            ref_of_ty_or_desc lower_loc lower_desc;
            text " are not exactly the same as those of ";
            ref_of_ty_or_desc upper_loc upper_desc;
          ])
    | MessageIncompatibleMappedTypeKey { source_type; mapped_type } ->
      [
        ref source_type;
        text " is incompatible with ";
        code "string | number | symbol";
        text ", so it cannot be used to generate keys for ";
        ref mapped_type;
      ]
    | MessageIncompatibleNonLiteralArrayToTuple { lower; upper } ->
      [
        ref lower;
        text " has an unknown number of elements, so is ";
        text "incompatible with ";
        ref upper;
      ]
    | MessageIncompatibleNonTypeGuardToTypeGuard { lower; upper } ->
      [
        ref lower;
        text ", a non-type-guard function, is incompatible with ";
        ref upper;
        text ", which is a type-guard function";
      ]
    | MessageIncompatibleReactDeepReadOnly { lower; upper; dro_loc } ->
      let react_runtime_str = "React runtime" in
      let (lower, react_runtime) =
        if Loc.equal (loc_of_reason lower |> loc_of_aloc) (loc_of_reason upper |> loc_of_aloc) then
          (mk_reason (desc_of_reason lower) dro_loc, text react_runtime_str)
        else
          (lower, hardcoded_string_desc_ref react_runtime_str dro_loc)
      in
      [
        ref lower;
        text " is managed by the ";
        react_runtime;
        text " and cannot be mutated, while ";
        ref upper;
        text " may allow mutations (possibly in nested values)";
      ]
    | MessageIncompatibleReactHooksDueToUniqueness { lower; upper } ->
      [ref lower; text " and "; ref upper; text " are different React hooks"]
    | MessageIncompatibleReactHooksWithNonReactHook { lower; upper; lower_is_hook; hook_is_annot }
      ->
      let (lower, upper) =
        let hook_wording =
          if hook_is_annot then
            text "hook type annotation"
          else
            text "hook"
        in
        if lower_is_hook then
          ([ref lower; text " is a React "; hook_wording], [ref upper; text " is not a hook"])
        else
          ([ref lower; text " is not a React hook"], [ref upper; text " is a "; hook_wording])
      in
      lower @ [text " but "] @ upper
    | MessageIncompatibleWithExact { kind; lower; upper } ->
      let object_kind =
        match kind with
        | UnexpectedIndexer -> "indexed "
        | UnexpectedInexact -> "inexact "
      in
      [text object_kind; ref lower; text " is incompatible with exact "; ref upper]
    | MessageIncompatibleWithIndexed { lower; upper } ->
      [ref lower; text " is incompatible with indexed "; ref upper]
    | MessageIncompleteExhausiveCheckEnum
        { description; enum_reason; left_to_check; default_case_loc } ->
      let left_to_check_features =
        match left_to_check with
        | [member_to_check] ->
          [text "the member "; code member_to_check; text " of enum "; ref enum_reason; text " has"]
        | _ ->
          let number_to_check = List.length left_to_check in
          let members_features =
            if number_to_check > 5 then
              let max_display_amount = 4 in
              (Base.List.take left_to_check max_display_amount
              |> Base.List.bind ~f:(fun member -> [code member; text ", "])
              )
              @ [text (spf "and %d others" (number_to_check - max_display_amount))]
            else
              Flow_errors_utils.Friendly.conjunction_concat
                (Base.List.map ~f:(fun member -> [code member]) left_to_check)
          in
          (text "the members " :: members_features)
          @ [text " of enum "; ref enum_reason; text " have"]
      in
      let default_features =
        match default_case_loc with
        | Some default_reason ->
          [
            text " The ";
            hardcoded_string_desc_ref "default case" default_reason;
            text " does not check for the missing members as the ";
            code (Lints.string_of_kind Lints.RequireExplicitEnumSwitchCases);
            text " lint has been enabled.";
          ]
        | None -> []
      in
      (text "Incomplete exhaustive check: " :: left_to_check_features)
      @ [text " not been considered in check of "; desc description; text "."]
      @ default_features
    | MessageIncorrectType kind ->
      let open IncorrectType in
      let incorrect_name = incorrect_of_kind kind in
      let replacement_name = replacement_of_kind kind in
      (match error_type_of_kind kind with
      | DeprecatedUtility ->
        [
          text "The utility type ";
          code incorrect_name;
          text " is deprecated, use ";
          code replacement_name;
          text " instead.";
        ]
        @
        (match kind with
        | DollarKeys -> [text " For example, `type KeyType = keyof MyObjectType;` "]
        | _ -> [])
      | TSType ->
        [
          text "The equivalent of TypeScript's ";
          code incorrect_name;
          text " type in Flow is ";
          code replacement_name;
          text ".";
        ])
    | MessageInternalType DollarReactDeepReadOnly ->
      [
        code "$ReactDeepReadOnly";
        text " is a secret internal Flow type exposed for testing purposes. ";
        text "There will be no stability guarantees.";
      ]
    | MessageInternalType (DollarUtilityTypeWithNonDollarAliases name) ->
      [code ("$" ^ name); text " is an internal Flow type. Use "; code name; text " instead."]
    | MessageInternalType (ReactDollarUtilityTypesWithNonDollarAliases name) ->
      [
        code ("React$" ^ name);
        text " is an internal Flow type. Use ";
        code ("React." ^ name);
        text " instead.";
      ]
    | MessageInvalidArgument { lower; upper } ->
      [ref lower; text " is not a valid argument of "; ref upper]
    | MessageInvalidCatchParameterAnnotation { ts_utility_syntax } ->
      [
        text "Invalid catch parameter type annotation. ";
        text "Annotation must be ";
        code "any";
        text " or ";
        (match ts_utility_syntax with
        | true -> code "unknown"
        | false -> code "mixed");
        text " if specified.";
      ]
    | MessageInvalidComponentRestParam ->
      [text "You may only use an identifier or a destructured object as a component rest param."]
    | MessageInvalidGenericRef typename ->
      [
        text "Cannot compare the result of ";
        code "typeof";
        text " to string ";
        text "literal ";
        code typename;
        text " because it is not a valid ";
        code "typeof";
        text " return value.";
      ]
    | MessageInvalidEnumMemberCheck { enum_reason; example_member; from_match } ->
      let suggestion =
        match enum_name_of_reason enum_reason with
        | Some enum_name ->
          let example_member = Base.Option.value ~default:"A" example_member in
          let (prefix, suffix) =
            if from_match then
              ("", "")
            else
              ("case ", ":")
          in
          [
            text " ";
            text "For example ";
            code (spf "%s%s.%s%s" prefix enum_name example_member suffix);
            text ".";
          ]
        | None -> []
      in
      let at =
        if from_match then
          "match pattern"
        else
          "case"
      in
      [
        text (spf "Invalid enum member check at %s. " at);
        text "The format must be dot-access of a member of ";
        ref enum_reason;
        text ".";
      ]
      @ suggestion
    | MessageInvalidGraphQL Graphql.InvalidTaggedTemplate ->
      [text "Template literal substitutions are not allowed in GraphQL literals."]
    | MessageInvalidGraphQL Graphql.InvalidGraphQL ->
      [text "Expected a GraphQL fragment, query, mutation, or subscription."]
    | MessageInvalidHookNaming ->
      [
        text "Hooks must have names that begin with ";
        code "use";
        text ". ";
        text "(https://react.dev/reference/rules/rules-of-hooks)";
      ]
    | MessageInvalidImportStarUse import_star_reason ->
      [
        text "The default export of a module cannot be accessed from an ";
        ref import_star_reason;
        text " object. To use the default export you must import it directly.";
      ]
    | MessageInvalidMappedTypeInInterfaceOrDeclaredClass ->
      [text "Mapped Types are not supported in interfaces or declared classes."]
    | MessageInvalidMappedTypeWithExactOrInexact ->
      [
        text "Mapped Types take on the exactness of the argument passed to keyof. They do not ";
        text "support explicit exact or inexact syntax.";
      ]
    | MessageInvalidMappedTypeWithExtraProps ->
      [text "Mapped Types cannot be used when other properties or indexers are present."]
    | MessageInvalidMappedTypeWithOptionalityRemoval ->
      [text "Mapped Types do not yet support optionality removal."]
    | MessageInvalidMappedTypeWithVarianceOnArrayInput ->
      [text "Mapped Types do not yet support variance annotations on array inputs."]
    | MessageInvalidInferType ->
      [
        text "Invalid infer type declaration. ";
        code "infer";
        text " declarations are only permitted in the ";
        code "extends";
        text " clause of a conditional type.";
      ]
    | MessageInvalidLintSettings kind ->
      (match kind with
      | LintSettings.Redundant_argument ->
        [text "Redundant argument. This argument doesn't change any lint settings."]
      | LintSettings.Overwritten_argument ->
        [
          text "Redundant argument. The values set by this argument are ";
          text "overwritten later in this comment.";
        ]
      | LintSettings.Naked_comment ->
        [text "Malformed lint rule. At least one argument is required."]
      | LintSettings.Nonexistent_rule ->
        [
          text "Nonexistent/misspelled lint rule. Perhaps you have a ";
          text "missing/extra ";
          code ",";
          text "?";
        ]
      | LintSettings.Invalid_setting ->
        [text "Invalid setting. Valid settings are error, warn, and off."]
      | LintSettings.Malformed_argument ->
        [
          text "Malformed lint rule. Properly formed rules contain a single ";
          code ":";
          text " character. Perhaps you have a missing/extra ";
          code ",";
          text "?";
        ])
    | MessageInvalidReactCreateElement invalid_react ->
      [
        text "Cannot create react element because the ";
        code "createElement";
        text " property of ";
        ref invalid_react;
        text " is incompatible with builtin ";
        code "React.createElement";
        text " type. ";
        text "Please check the ";
        ref invalid_react;
        text " identifier in scope to ensure it is the right one.";
      ]
    | MessageInvalidRefPropertyInSpread { ref_loc; spread_loc } ->
      [
        text "Components do not support ";
        hardcoded_string_desc_ref "ref properties" ref_loc;
        text " within ";
        hardcoded_string_desc_ref "spreads" spread_loc;
      ]
    | MessageInvalidKeyPropertyInSpread { key_loc; spread_loc } ->
      [
        text "Cannot ";
        hardcoded_string_desc_ref "spread" spread_loc;
        text " an object that contains a ";
        hardcoded_string_desc_ref "`key` property" key_loc;
      ]
    | MessageInvalidSelfReferencingTypeAnnotation { name; loc } ->
      [
        text "Invalid type annotation for ";
        code name;
        text ". It contains a ";
        hardcoded_string_desc_ref "reference" loc;
        text " to the binding being declared.";
      ]
    | MessageInvalidSelfReferencingDefault { name; def_loc; ref_loc } ->
      [
        text "Invalid default expression for parameter ";
        hardcoded_string_desc_ref name def_loc;
        text ". It contains a ";
        hardcoded_string_desc_ref "reference" ref_loc;
        text " to the binding being declared.";
      ]
    | MessageInvalidTrivialRecursiveDefinition description ->
      [text "Invalid trivially recursive definition of "; desc description; text ". "]
    | MessageInvalidTupleRequiredAfterOptional { reason_tuple; reason_required; reason_optional } ->
      [
        text "Invalid ";
        ref reason_tuple;
        text ", required ";
        ref reason_required;
        text " must be after optional ";
        ref reason_optional;
        text ".";
      ]
    | MessageInvalidTupleTypeSpread reason_arg ->
      [text "Cannot spread non-tuple ("; ref reason_arg; text ") into tuple type."]
    | MessageTupleElementAfterInexactSpread ->
      [text "Cannot have element after spread of inexact tuple."]
    | MessageInvalidRendersTypeArgument
        { renders_variant; invalid_render_type_kind; invalid_type_reasons } ->
      let additional_explanation =
        match (invalid_render_type_kind, renders_variant) with
        | (InvalidRendersStructural r, _) ->
          [
            text " You can only use an element of ";
            code "AbstractComponent";
            text " when the third type argument is a render type and ";
            ref r;
            text " is not a render type.";
          ]
        | (InvalidRendersNonNominalElement r, _) ->
          [
            text " Only elements of a component-syntax components can appear in renders but ";
            ref r;
            text " is not a component-syntax component.";
          ]
        | (InvalidRendersNullVoidFalse, Flow_ast.Type.Renders.Maybe) ->
          [
            text " Only elements of a component-syntax components can appear in renders. ";
            code "renders?";
            text " already includes React nodes that render nothing.";
          ]
        | (InvalidRendersNullVoidFalse, Flow_ast.Type.Renders.Star) ->
          [
            text " Only elements of a component-syntax components can appear in renders. ";
            code "renders*";
            text " already includes React nodes that render nothing.";
          ]
        | (InvalidRendersIterable, Flow_ast.Type.Renders.Star) ->
          [
            text " Only elements of a component-syntax components can appear in renders. ";
            code "renders*";
            text
              " already models rendering any amount of children in all possible nesting structures.";
          ]
        | (InvalidRendersNullVoidFalse, _) ->
          [
            text " Only elements of a component-syntax components can appear in renders. ";
            text "If you want to express the idea of rendering zero or one item, please use ";
            code "renders?";
            text " instead.";
          ]
        | (InvalidRendersIterable, _) ->
          [
            text " Only elements of a component-syntax components can appear in renders. ";
            text "If you want to express the idea of rendering zero or more items, please use ";
            code "renders*";
            text " instead.";
          ]
        | (InvalidRendersGenericT, Flow_ast.Type.Renders.Star) ->
          [text " Generic render types are not allowed with "; code "renders*"; text "."]
        | (InvalidRendersGenericT, _) ->
          [
            text " Generic render types are only allowed in rendering declaration of ";
            text
              "component syntax components, and only one or a union of generic types are permitted.";
          ]
        | (UncategorizedInvalidRenders, _) -> []
      in
      let rec refs = function
        | (r, []) -> [ref r]
        | (r1, [r2]) -> [ref r1; text " and "; ref r2]
        | (r1, r2 :: rs) -> [ref r1; text ", "] @ refs (r2, rs)
      in
      [text "Cannot use "]
      @ refs invalid_type_reasons
      @ [text " as the type argument of renders type."]
      @ additional_explanation
    | MessageInvalidTypeCastingSyntax enabled_casting_syntax ->
      let (valid, invalid) = type_casting_examples enabled_casting_syntax in
      [
        text "Invalid type cast syntax. Use the form ";
        code valid;
        text " instead of the form ";
        code invalid;
        text ".";
      ]
    | MessageInvalidTypeGuardFunctionKind kind ->
      [text "Cannot declare a type guard on a(n) "; text kind; text " function."]
    | MessageInvalidTypeGuardFunctionWritten { type_guard_reason; write_locs } ->
      let loc_str =
        match write_locs with
        | [] -> [text "in this function"]
        | [loc] -> [text "in"; no_desc_ref loc]
        | _ -> text "in the following statements:" :: Base.List.map write_locs ~f:no_desc_ref
      in
      [
        text "Cannot use ";
        ref type_guard_reason;
        text " because at this return point it is written to ";
      ]
      @ loc_str
      @ [text "."]
    | MessageInvalidTypeGuardParamUnbound reason ->
      [text "Cannot find "; ref reason; text " in the parameters of this function (type)."]
    | MessageInvalidTypeGuardThisParam reason ->
      [
        text "Cannot use ";
        ref reason;
        text " as a type guard variable in this context. ";
        code "this";
        text " type guards are only supported in non-static declare class or interface methods.";
      ]
    | MessageInvalidUseOfFlowEnforceOptimized arg ->
      [text "Invalid use of $Flow$EnforceOptimized on non-union type "; ref arg; text "."]
    | MessageMissingAnnotation d -> [text "Missing an annotation on "; desc d; text "."]
    | MessageMissingAnnotationDueToContextualTypingFailure d ->
      [
        text "An annotation on ";
        desc d;
        text " is required because Flow cannot infer its type from local context.";
      ]
    | MessageMissingAnnotationForGenericFunction d ->
      [
        text "Missing an annotation on ";
        desc d;
        text " because generic functions must be fully annotated.";
      ]
    | MessageLowerIsNotArray lower -> [ref lower; text " is not an array"]
    | MessageLowerIsNotArrayIndex lower -> [ref lower; text " is not an array index"]
    | MessageLowerIsNotClass lower -> [ref lower; text " is not a class"]
    | MessageLowerIsNotClassWithPrivateProps lower ->
      [ref lower; text " is not a class with private properties"]
    | MessageLowerIsNotFunction lower -> [ref lower; text " is not a function"]
    | MessageLowerIsNotFunctionType lower -> [ref lower; text " is not a function type"]
    | MessageLowerIsNotInheritable lower -> [ref lower; text " is not inheritable"]
    | MessageLowerIsNotInstanceType lower -> [ref lower; text " is not an instance type"]
    | MessageLowerIsNotObject lower -> [ref lower; text " is not an object"]
    | MessageLowerIsNotPolymorphicType lower -> [ref lower; text " is not a polymorphic type"]
    | MessageLowerIsNotReactComponent lower -> [ref lower; text " is not a React component"]
    | MessageLowerIsNotSupportedByUnclassifiedUse { lower; ctor } ->
      [ref lower; text " is not supported by unclassified use "; text ctor]
    | MessageMethodUnbinding { reason_op; context_loc } ->
      [
        ref reason_op;
        text " cannot be unbound from the ";
        Friendly.(Reference ([Text "context"], loc_of_aloc context_loc));
        text " where it was defined";
      ]
    | MessageMissingPlatformSupportWithAvailablePlatforms
        { available_platforms; required_platforms } ->
      let missing_platforms = SSet.diff required_platforms available_platforms |> SSet.elements in
      let platform_features = function
        | [] -> [text "no platforms"]
        | [p] -> [text "the "; code p; text " platform"]
        | p1 :: ps ->
          let rec loop = function
            | [] -> []
            | p :: rest -> text ", " :: code p :: loop rest
          in
          text "the following platforms: " :: code p1 :: loop ps
      in
      [text "The imported module supports "]
      @ platform_features (SSet.elements available_platforms)
      @ [text ", but the current module requires the support of "]
      @ platform_features (SSet.elements required_platforms)
      @ [text ". Support for "]
      @ platform_features missing_platforms
      @ [text " is missing."]
    | MessageMissingPlatformSupport { missing_platforms } ->
      let platform_features = function
        | [] -> [text "no platforms"]
        | [p] -> [text "the "; code p; text " platform"]
        | p1 :: ps ->
          let rec loop = function
            | [] -> []
            | p :: rest -> text ", " :: code p :: loop rest
          in
          text "the following platforms: " :: code p1 :: loop ps
      in
      [text "Support for "]
      @ platform_features (SSet.elements missing_platforms)
      @ [text " is missing."]
    | MessageNoDefaultExport { module_name; suggestion } ->
      [
        text "Cannot import a default export because there is no default export ";
        text "in ";
        code module_name;
        text ".";
      ]
      @
      (match suggestion with
      | None -> []
      | Some suggestion ->
        [
          text " ";
          text "Did you mean ";
          code (spf "import {%s} from \"%s\"" suggestion module_name);
          text "?";
        ])
    | MessageNoNamedExport { module_name; export_name; suggestion } ->
      [
        text "Cannot import ";
        code export_name;
        text " because ";
        text "there is no ";
        code export_name;
        text " export in ";
        code module_name;
        text ".";
      ]
      @
      (match suggestion with
      | None -> []
      | Some suggestion -> [text " Did you mean "; code suggestion; text "?"])
    | MessageNonConstVarExport decl_reason ->
      let reason_opt =
        match decl_reason with
        | Some reason -> [text "variable "; ref reason]
        | None -> [text "variable"]
      in
      List.concat
        [
          [text "Cannot export "];
          reason_opt;
          [
            text " declared using ";
            code "var";
            text " or ";
            code "let";
            text ". All exported variables must be ";
            code "const";
            text ".";
          ];
        ]
    | MessageNonStrictImport ->
      [
        text "Dependencies of a ";
        code "@flow strict";
        text " module must ";
        text "also be ";
        code "@flow strict";
        text "!";
      ]
    | MessageNonToplevelExport -> [text "Exports can only appear at the top level"]
    | MessageOnlyDefaultExport { module_name; export_name } ->
      [
        text "Cannot import ";
        code export_name;
        text " because ";
        text "there is no ";
        code export_name;
        text " export in ";
        code module_name;
        text ". Did you mean ";
        code (spf "import %s from \"...\"" export_name);
        text "?";
      ]
    | MessageParseError parse_error ->
      Flow_errors_utils.Friendly.message_of_string (Parse_error.PP.error parse_error)
    | MessagePlatformSpecificImplementationModuleLookupFailed name ->
      [
        text "Cannot resolve platform-specific implementation module ";
        code name;
        text ". ";
        text "All platform-specific implementations must exist for this interface. ";
        text "Read the docs on Flow's multi-platform support for more information: ";
        text "https://flow.org/en/docs/react/multiplatform";
      ]
    | MessagePropExtraAgainstExactObject { lower; upper; props = (first_prop, rest_props) } ->
      let prop_message =
        let props = first_prop :: rest_props in
        let number_to_check = List.length props in
        if number_to_check > 5 then
          let max_display_amount = 4 in
          (Base.List.take props max_display_amount
          |> Base.List.bind ~f:(fun prop -> mk_prop_message (Some prop) @ [text ", "])
          )
          @ [text (spf "and %d others" (number_to_check - max_display_amount))]
        else
          Flow_errors_utils.Friendly.conjunction_concat
            (Base.List.map (first_prop :: rest_props) ~f:(fun prop -> mk_prop_message (Some prop)))
      in
      let plural =
        match rest_props with
        | [] -> false
        | _ :: _ -> true
      in
      let upper_kind =
        if is_record_reason upper then
          "Records"
        else
          "Exact objects"
      in
      prop_message
      @ [
          text " ";
          text
            ( if plural then
              "are"
            else
              "is"
            );
          text " extra in ";
          ref lower;
        ]
      @ [text " but missing in "]
      @ [ref upper]
      @ [text ". "; text upper_kind; text " do not accept extra props"]
    | MessagePropMissing { lower; upper; prop; suggestion; reason_indexer } ->
      (* If we were subtyping that add to the error message so our user knows what
       * object required the missing property. *)
      let prop_message = mk_prop_message prop in
      let indexer_message =
        match reason_indexer with
        | None -> []
        | Some indexer ->
          [
            text ". Any property that does not exist in ";
            ref lower;
            text " must be compatible with its indexer ";
            ref indexer;
          ]
      in

      let suggestion =
        match suggestion with
        | Some suggestion -> [text " (did you mean "; code suggestion; text "?)"]
        | None -> []
      in
      (match upper with
      | Some upper ->
        prop_message
        @ suggestion
        @ [text " is missing in "; ref lower]
        @ [text " but exists in "]
        @ [ref upper]
        @ indexer_message
      | None ->
        (match prop with
        | None when is_nullish_reason lower -> [ref lower; text " does not have properties"]
        | _ -> prop_message @ suggestion @ [text " is missing in "; ref lower]))
    | MessagePropsMissing { lower; upper; props } ->
      (match props with
      | (prop, []) ->
        [
          text "property ";
          code prop;
          text " is missing in ";
          ref lower;
          text " but exists in ";
          ref upper;
        ]
      | _ ->
        [text "properties "]
        @ Flow_errors_utils.Friendly.conjunction_concat
            (Base.List.map (Nel.to_list props) ~f:(fun p -> [code p]))
        @ [text " are missing in "; ref lower; text " but exist in "; ref upper])
    | MessagePropPolarityMismatch { lower; upper; props } ->
      let f (prop, lpole, upole) =
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
      in
      let props =
        Nel.to_list props |> Base.List.sort ~compare:(fun (a, _, _) (b, _, _) -> Stdlib.compare a b)
      in
      let number_to_check = List.length props in
      let props =
        if number_to_check > 5 then
          let max_display_amount = 4 in
          let cropped_props = Base.List.take props max_display_amount in
          let others =
            [
              text
                (spf "%d other properties are incompatible" (number_to_check - max_display_amount));
            ]
          in
          Base.List.map ~f cropped_props @ [others]
        else
          Base.List.map ~f props
      in
      Flow_errors_utils.Friendly.conjunction_concat props
    | MessagePropNotReadable x ->
      mk_prop_message (Base.Option.map ~f:display_string_of_name x) @ [text " is not readable"]
    | MessagePropNotWritable x ->
      mk_prop_message (Base.Option.map ~f:display_string_of_name x) @ [text " is not writable"]
    | MessageReactIntrinsicOverlap { use; def; type_; mixed } ->
      [
        text "The name of intrinsic element ";
        ref use;
        text " overlaps with a ";
        hardcoded_string_desc_ref "local definition" def;
        text " which has a ";
        hardcoded_string_desc_ref "type" type_;
        text " that ";
        ( if mixed then
          text "may"
        else
          text "can"
        );
        text
          " be instantiated as an element. To avoid confusion between this definition and the intrinsic, rename the definition";
      ]
    | MessageReadonlyArraysCannotBeWrittenTo -> [text "read-only arrays cannot be written to"]
    | MessageRecursionLimitExceeded -> [text "*** Recursion limit exceeded ***"]
    | MessageRedeclareComponentProp { duplicates = ((first_loc, name, second_loc), []); spread_loc }
      ->
      let first = mk_reason (RIdentifier name) first_loc in
      [
        text "Component property ";
        ref first;
        text " is ";
        hardcoded_string_desc_ref "re-declared" second_loc;
        text " within a ";
        hardcoded_string_desc_ref "spread" spread_loc;
        text ". Property names may only be have one definition within a component.";
      ]
    | MessageRedeclareComponentProp { duplicates; spread_loc } ->
      let individual_redeclares_msgs =
        duplicates
        |> Nel.to_list
        |> Base.List.concat_map ~f:(fun (first_loc, name, second_loc) ->
               let first = mk_reason (RIdentifier name) first_loc in
               [
                 text " - ";
                 ref first;
                 text " is re-declared ";
                 hardcoded_string_desc_ref "here" second_loc;
                 text "\n";
               ]
           )
      in
      [
        text "Multiple component properties are re-declared within a ";
        hardcoded_string_desc_ref "spread" spread_loc;
        text ".\n";
      ]
      @ individual_redeclares_msgs
      @ [text "Property names may only be have one definition within a component."]
    | MessageShouldAnnotateVariableOnlyInitializedInGenericContext
        { reason; possible_generic_escape_locs } ->
      [
        text "Variable ";
        ref reason;
        text " should be annotated, because it is only initialized in a generic context";
      ]
      @ (Base.List.map possible_generic_escape_locs ~f:no_desc_ref
        |> Base.List.intersperse ~sep:(text ",")
        )
    | MessageShouldAnnotateVariableUsedInGenericContext
        { reason; null_loc; initialized; possible_generic_escape_locs } ->
      let null_ref =
        if initialized then
          code "null"
        else
          ref (mk_reason (RCode "null") null_loc)
      in
      [
        text "Variable ";
        ref reason;
        text " should be annotated, because it is only ever assigned to by ";
        null_ref;
        text " and in generic context";
      ]
      @ (Base.List.map possible_generic_escape_locs ~f:no_desc_ref
        |> Base.List.intersperse ~sep:(text ",")
        )
    | MessageShouldNotBeCoerced lower -> [ref lower; text " should not be coerced"]
    | MessageShouldUseArrayLiteral ->
      [text "Use an array literal instead of "; code "new Array(...)"; text "."]
    | MessageSketchyNumber reason ->
      [
        text "Avoid using ";
        code "&&";
        text " to check the value of ";
        ref reason;
        text ". ";
        text "Consider handling falsy values (0 and NaN) by using a conditional to choose an ";
        text "explicit default instead.";
      ]
    | MessageSketchyNullCheck { kind; falsy_loc; null_loc } ->
      let (type_str, value_str) =
        match kind with
        | Lints.SketchyNullBool -> ("boolean", "false")
        | Lints.SketchyNullNumber -> ("number", "0")
        | Lints.SketchyNullBigInt -> ("bigint", "0n")
        | Lints.SketchyNullString -> ("string", "an empty string")
        | Lints.SketchyNullMixed -> ("mixed", "false")
        | Lints.SketchyNullEnumBool -> ("boolean enum", "false at runtime")
        | Lints.SketchyNullEnumNumber -> ("number enum", "0 at runtime")
        | Lints.SketchyNullEnumBigInt -> ("bigint enum", "0n at runtime")
        | Lints.SketchyNullEnumString -> ("string enum", "an empty string at runtime")
      in
      [
        text "Sketchy null check on ";
        hardcoded_string_desc_ref type_str falsy_loc;
        text " ";
        text "which is potentially ";
        text value_str;
        text ". Perhaps you meant to ";
        text "check for ";
        ref (mk_reason RNullOrVoid null_loc);
        text "?";
      ]
    | MessageSuppressionMalformedCode ->
      [
        text "Suppression contains a malformed error code. Suppressions with error codes ";
        text "should be formatted as ";
        code "$FlowFixMe[<CODE>]";
        text ".";
      ]
    | MessageSuppressionMissingCode ->
      [
        text "Suppression is missing a code. Please update this suppression to use an error code: ";
        code "$FlowFixMe[<CODE>]";
      ]
    | MessageThisInComponent component_loc ->
      [
        text "Cannot reference ";
        code "this";
        text " from within ";
        hardcoded_string_desc_ref "component declaration" component_loc;
      ]
    | MessageThisInExportedFunction ->
      [text "Cannot use "; code "this"; text " in an exported function."]
    | MessageThisSuperInObject (reason, kind) ->
      let (v, suggestion) =
        match kind with
        | This_finder.This ->
          ( "this",
            [
              text " Consider replacing the reference to ";
              code "this";
              text " with the name of the object, or rewriting the object as a class.";
            ]
          )
        | This_finder.Super -> ("super", [text " Consider rewriting the object as a class."])
      in
      [
        text "Cannot reference ";
        code v;
        text " from within ";
        ref reason;
        text ". For safety, Flow restricts access to ";
        code v;
        text " inside object methods since these methods may be unbound and rebound.";
      ]
      @ suggestion
    | MessageTSKeyofType ->
      [
        code "keyof";
        text " is only supported when used inline in a mapped type. ";
        text "The equivalent of TypeScript's ";
        code "keyof";
        text " type operator in Flow is the ";
        code "$Keys";
        text " utility type, used in the form ";
        code "$Keys<T>";
        text ".";
      ]
    | MessageTSNeverType ->
      [
        text "The closest equivalent of TypeScript's ";
        code "never";
        text " type in Flow is ";
        code "empty";
        text ".";
      ]
    | MessageTSParamExtends ->
      [
        text "While TypeScript uses ";
        code "extends";
        text " to specify type parameter bounds, Flow uses ";
        code ":";
        text " in the form ";
        code "type T<A: B> = ...";
        text ".";
      ]
    | MessageTSReadonlyOperatorOnArray ->
      [
        text "The equivalent of TypeScript's ";
        code "readonly";
        text " type operator applied to an array type is ";
        code "ReadonlyArray<T>";
        text ".";
      ]
    | MessageTSReadonlyOperatorOnTuple ->
      [
        text "The equivalent of TypeScript's ";
        code "readonly";
        text " type operator applied to a tuple type is ";
        code "Readonly<[T, S]>";
        text ".";
      ]
    | MessageTSReadonlyType ->
      [
        text "TypeScript's ";
        code "readonly";
        text " type operator is not valid in Flow. ";
        text "For array types, you can use ";
        code "ReadonlyArray<T>";
        text ". For object and tuple types you can use ";
        code "Readonly<T>";
        text ".";
      ]
    | MessageTSClassAccessibility kind ->
      let (modifier, suffix) =
        match kind with
        | Flow_ast.Class.TSAccessibility.Private ->
          ( "private",
            [
              text "Use JavaScript private elements instead. To fix, change ";
              code "private foo";
              text " to ";
              code "#foo";
              text ".";
            ]
          )
        | Flow_ast.Class.TSAccessibility.Public ->
          ( "public",
            [
              text "Fields and methods are public by default. To fix, remove the ";
              code "public";
              text " modifier.";
            ]
          )
        | Flow_ast.Class.TSAccessibility.Protected ->
          ("protected", [text "To fix, remove the "; code "protected"; text " modifier."])
      in
      [text "Flow does not support using "; code modifier; text " in classes. "] @ suffix
    | MessageTSParameterProperty ->
      [
        text "Flow does not support TypeScript parameter properties. ";
        text "To fix, declare the property in the class body and assign it in the constructor.";
      ]
    | MessageAbstractClass -> [text "Flow does not support "; code "abstract"; text " classes."]
    | MessageAbstractMethod -> [text "Flow does not support "; code "abstract"; text " methods."]
    | MessageTSSatisfiesType enabled_casting_syntax ->
      let (example, _) = type_casting_examples enabled_casting_syntax in
      [
        text "The closest equivalent of TypeScript's ";
        code "satisfies";
        text " expression in Flow is to do a cast in the form ";
        code example;
        text ".";
      ]
    | MessageTSVarianceIn ->
      [
        text "The equivalent of TypeScript's ";
        code "in";
        text " variance annotation is ";
        code "-";
        text " in Flow.";
      ]
    | MessageTSVarianceInOut ->
      [
        text "The equivalent of TypeScript's ";
        code "in out";
        text " variance annotation in Flow is to simply leave it out - ";
        text "it's the default if you don't have a variance annotation.";
      ]
    | MessageTSVarianceOut ->
      [
        text "The equivalent of TypeScript's ";
        code "out";
        text " variance annotation is ";
        code "+";
        text " in Flow.";
      ]
    | MessageTSVarianceReadOnly ->
      [
        text "While TypeScript uses ";
        code "readonly";
        text " to specify read only properties, Flow uses ";
        code "+";
        text " in the form ";
        code "+foo: T";
        text " for class and object type properties, and ";
        code "+[string]: T";
        text " for dictionaries.";
      ]
    | MessageTSUndefinedType ->
      [
        text "The equivalent of TypeScript's ";
        code "undefined";
        text " type in Flow is ";
        code "void";
        text ". ";
        text "Flow does not have separate ";
        code "void";
        text " and ";
        code "undefined";
        text " types.";
      ]
    | MessageTSUnknownType ->
      [
        text "The equivalent of TypeScript's ";
        code "unknown";
        text " type in Flow is ";
        code "mixed";
        text ".";
      ]
    | MessageUnclearType ->
      [
        text "Unclear type. Using ";
        code "any";
        text ", ";
        code "Object";
        text ", or ";
        code "Function";
        text " types is not safe!";
      ]
    | MessageUnderconstrainedImplicitInstantiaton { reason_call; reason_tparam } ->
      [
        ref reason_tparam;
        text " is underconstrained by ";
        ref reason_call;
        text ". Either add explicit type arguments or cast the expression to your expected type";
      ]
    | MessageUndocumentedFeature ->
      [
        text "You are using an undocumented feature. ";
        text "It might be removed in the future, ";
        text "and the behavior might change at any time without warning.";
      ]
    | MessageIllegalAssertOperator { obj; specialized } ->
      let explanation =
        if specialized then
          [text "The assert operator can only be used with arrays and objects with indexers."]
        else
          [text "The assert operator can only be applied to values with nullable types."]
      in
      [
        ref obj;
        text " is not a valid target of the nonnull assertion operator (";
        code "!";
        text "). ";
      ]
      @ explanation
    | MessageTupleElementNotReadable { reason; index; name } ->
      mk_tuple_element_error_message loc_of_aloc ~reason ~index ~name "readable"
    | MessageTupleElementNotWritable { reason; index; name } ->
      mk_tuple_element_error_message loc_of_aloc ~reason ~index ~name "writable"
    | MessageTupleIndexOutOfBound { reason_op; inexact; length; index } ->
      [
        ref reason_op;
        text
          (spf
             " only has %d element%s%s, so index %s is %s"
             length
             ( if length == 1 then
               ""
             else
               "s"
             )
             ( if inexact then
               " explicitly defined"
             else
               ""
             )
             index
             ( if inexact then
               "unknown or out of bounds"
             else
               "out of bounds"
             )
          );
      ]
    | MessageTupleNonIntegerIndex { index_def_loc; index } ->
      [
        text "the index into a tuple must be an integer, but ";
        Friendly.(Reference ([Code index], loc_of_aloc index_def_loc));
        text " is not an integer";
      ]
    | MessageTupleNonStaticallyKnownIndex ->
      [text "the index must be statically known to write a tuple element"]
    | MessageTuplePolarityMismatch
        { index; reason_lower; reason_upper; polarity_lower; polarity_upper } ->
      let expected = polarity_explanation (polarity_lower, polarity_upper) in
      let actual = polarity_explanation (polarity_upper, polarity_lower) in
      [
        text "tuple element at index ";
        code (string_of_int index);
        text " is ";
        text expected;
        text " in ";
        ref reason_lower;
        text " but ";
        text actual;
        text " in ";
        ref reason_upper;
      ]
    | MessageTypeGuardIndexMismatch { lower; upper } ->
      [ref lower; text " does not appear in the same position as "; ref upper]
    | MessageTypeGuardImpliesMismatch { lower; upper } ->
      [text "one-sided "; ref lower; text " is incompatible with default "; ref upper]
    | MessageNegativeTypeGuardConsistency { reason = _; return_reason; type_reason } ->
      [
        text "Cannot return ";
        desc (Reason.desc_of_reason return_reason);
        text " because the negation of the predicate encoded in this expression";
        text " needs to completely refine away the guard type ";
        ref type_reason;
        text ". ";
        text "Consider using a one-sided type-guard (`implies x is T`). ";
        text "See 2. in ";
        text
          "https://flow.org/en/docs/types/type-guards/#toc-consistency-checks-of-type-guard-functions.";
      ]
    | MessageUnexpectedUseOfThisType -> [text "Unexpected use of "; code "this"; text " type."]
    | MessageUninitializedInstanceProperty Lints.MethodCallBeforeEverythingInitialized ->
      [
        text "It is unsafe to call a method in the constructor before all ";
        text "class properties are definitely initialized.";
      ]
    | MessageUninitializedInstanceProperty Lints.PropertyFunctionCallBeforeEverythingInitialized ->
      [
        text "It is unsafe to call a property function in the constructor ";
        text "before all class properties are definitely initialized.";
      ]
    | MessageUninitializedInstanceProperty Lints.PropertyNotDefinitelyInitialized ->
      [
        text "Class property not definitely initialized in the constructor. ";
        text "Can you add an assignment to the property declaration?";
      ]
    | MessageUninitializedInstanceProperty Lints.ReadFromUninitializedProperty ->
      [
        text "It is unsafe to read from a class property before it is ";
        text "definitely initialized.";
      ]
    | MessageUninitializedInstanceProperty Lints.ThisBeforeEverythingInitialized ->
      [
        text "It is unsafe to use ";
        code "this";
        text " in the constructor ";
        text "before all class properties are definitely initialized.";
      ]
    | MessageUnknownParameterTypes lower ->
      [text "the parameter types of an "; ref lower; text " are unknown"]
    | MessageUnnecessaryDeclareTypeOnlyExport ->
      [text "The "; code "declare"; text " keyword is unnecessary for type exports."]
    | MessageUnnecessaryInvariant reason ->
      [
        text "This use of `invariant` is unnecessary because "; ref reason; text " is always truthy.";
      ]
    | MessageUnnecessaryOptionalChain lhs_reason ->
      [
        text "This use of optional chaining (";
        code "?.";
        text ") is unnecessary because ";
        ref lhs_reason;
        text " cannot be nullish or because an earlier ";
        code "?.";
        text " will short-circuit the nullish case.";
      ]
    | MessageUnreachableCode -> [text "Unreachable code."]
    | MessageUnsafeGetterSetter ->
      [text "Getters and setters can have side effects and are unsafe."]
    | MessageUnsafeObjectAssign ->
      [text "Flow's support for "; code "Object.assign"; text " is unsafe. Use spreads instead."]
    | MessageUnsupportedKeyInObject { key_error_kind; obj_kind } ->
      let suffix =
        Base.Option.value
          ~default:[text " Only identifier, string literal, and number literal keys are allowed."]
          (msg_of_invalid_obj_key_kind key_error_kind)
      in
      let obj_kind =
        match obj_kind with
        | `Type -> "type"
        | `Literal -> "literal"
      in
      [text "Unsupported key in object "; text obj_kind; text "."] @ suffix
    | MessageUnsupportedSyntax AnnotationInsideDestructuring ->
      [
        text "Annotations inside of destructuring are not supported. ";
        text "Annotate the top-level pattern instead. ";
        text "For example, instead of the invalid ";
        code "const [a: number, b: string] = ...";
        text " do ";
        code "const [a, b]: [number, string] = ...";
        text ".";
      ]
    | MessageUnsupportedSyntax AsConstOnNonLiteral ->
      [
        text "The ";
        code "as const";
        text " assertion can only be used on string, numeric, boolean, object, ";
        text "or array literals, ";
        text "or const-variables initialized with primitive literals.";
      ]
    | MessageUnsupportedSyntax CatchParameterDeclaration ->
      [text "Unsupported catch parameter declaration."]
    | MessageUnsupportedSyntax ClassPropertyLiteral ->
      [text "Literal properties not yet supported."]
    | MessageUnsupportedSyntax ClassPropertyComputed ->
      [text "Computed property keys not supported."]
    | MessageUnsupportedSyntax ClassStaticBlock -> [text "Class static blocks are not supported."]
    | MessageUnsupportedSyntax ClassDeclareMethod ->
      [text "Declare methods in classes are not supported."]
    | MessageUnsupportedSyntax ComponentSyntax ->
      [
        text "Component syntax is not enabled. ";
        text "You may opt-in to using component syntax by putting ";
        code "component_syntax=true";
        text " into the ";
        code "[options]";
        text " section of your ";
        code ".flowconfig";
        text ".";
      ]
    | MessageUnsupportedSyntax (ContextDependentUnsupportedStatement NonLibdefToplevelDeclareModule)
      ->
      [
        code "declare module"; text " statement is only supported at the toplevel of a library file.";
      ]
    | MessageUnsupportedSyntax (ContextDependentUnsupportedStatement ToplevelLibraryImport) ->
      [
        text "Cannot use an import statement at the toplevel of a library file. ";
        text "Import statements may only appear inside a ";
        code "declare module";
        text ". The statement will be ignored.";
      ]
    | MessageUnsupportedSyntax
        (ContextDependentUnsupportedStatement (UnsupportedStatementInLibdef kind)) ->
      [
        text "Cannot use ";
        text kind;
        text " statements in a library file. ";
        text "The statement will be ignored.";
      ]
    | MessageUnsupportedSyntax
        (ContextDependentUnsupportedStatement (UnsupportedStatementInDeclareModule kind)) ->
      [
        text "Cannot use ";
        text kind;
        text " statements with in ";
        code "declare module";
        text ". The statement will be ignored.";
      ]
    | MessageUnsupportedSyntax
        (ContextDependentUnsupportedStatement (UnsupportedStatementInDeclareNamespace kind)) ->
      [
        text "Cannot use ";
        text kind;
        text " statements with in ";
        code "declare namespace";
        text ". The statement will be ignored.";
      ]
    | MessageUnsupportedSyntax DeclareGlobal ->
      [code "declare global"; text " statement is not supported yet."]
    | MessageUnsupportedSyntax DestructuringExpressionPattern ->
      [text "Unsupported expression pattern in destructuring."]
    | MessageUnsupportedSyntax DestructuringObjectPropertyInvalidLiteral ->
      [
        text
          "Unsupported literal object property in destructuring. String literals and int-like number literals are supported.";
      ]
    | MessageUnsupportedSyntax ExistsType ->
      [
        text "The existential type ";
        code "*";
        text " is deprecated. This syntax is no longer supported.";
      ]
    | MessageUnsupportedSyntax ExplicitCallAfterProto ->
      [text "Unexpected call property after explicit prototype."]
    | MessageUnsupportedSyntax ExplicitProtoAfterCall ->
      [text "Unexpected prototype after call property."]
    | MessageUnsupportedSyntax IllegalName -> [text "Illegal name."]
    | MessageUnsupportedSyntax ImportDynamicArgument ->
      [text "The parameter passed to "; code "import"; text " must be a string literal."]
    | MessageUnsupportedSyntax InvariantSpreadArgument ->
      [text "Unsupported arguments in call to "; code "invariant"; text "."]
    | MessageUnsupportedSyntax JSXTypeArgs -> [text "Flow doesn't support JSX type arguments."]
    | MessageUnsupportedSyntax MatchExpression ->
      [code "match"; text " expressions are not supported."]
    | MessageUnsupportedSyntax MatchStatement ->
      [code "match"; text " statements are not supported."]
    | MessageUnsupportedSyntax MatchInstancePattern ->
      [code "match"; text " instance patterns are not supported."]
    | MessageUnsupportedSyntax MetaPropertyExpression -> [text "Not supported."]
    | MessageUnsupportedSyntax MultipleIndexers -> [text "Multiple indexers are not supported."]
    | MessageUnsupportedSyntax MultipleProtos -> [text "Multiple prototypes specified."]
    | MessageUnsupportedSyntax ObjectPropertyGetSet ->
      [text "Get/set properties not yet supported."]
    | MessageUnsupportedSyntax ObjectPropertyComputedGetSet ->
      [text "Computed getters and setters are not yet supported."]
    | MessageUnsupportedSyntax OpaqueTypeExtendsBound ->
      [
        text "Specifying opaque type upper bound using ";
        code "extends";
        text " keyword is not yet supported";
      ]
    | MessageUnsupportedSyntax OpaqueTypeSuperBound ->
      [
        text "Specifying opaque type lower bound using ";
        code "super";
        text " keyword is not yet supported";
      ]
    | MessageUnsupportedSyntax PredicateFunction ->
      [text "Support for predicate functions is removed. `%checks` declaration is now ignored."]
    | MessageUnsupportedSyntax PredicateDeclarationAnonymousParameters ->
      [text "Predicate function declarations cannot use anonymous "; text "function parameters."]
    | MessageUnsupportedSyntax Records -> [text "Records are not enabled."]
    | MessageUnsupportedSyntax DeclareClassMethodMissingReturnType ->
      [text "Return type annotation is required for "; code "declare class"; text " methods."]
    | MessageUnsupportedSyntax (TSLibSyntax kind) ->
      let kind_str =
        match kind with
        | DeclarationWithoutDeclare -> "Declaration without `declare`"
        | ImportTypeAnnotation -> "`import(\"module\")` type syntax"
        | DeclareExportNamespace -> "`declare export namespace`"
        | ExportAssignment -> "`export =` syntax"
        | ImportEqualsDeclaration -> "`import ... = ...` syntax"
        | ImportEqualsQualifiedName -> "`import ... = <QualifiedName>` syntax"
      in
      [text kind_str; text " is not enabled."]
    | MessageUnsupportedSyntax RequireDynamicArgument ->
      [text "The parameter passed to "; code "require"; text " must be a string literal."]
    | MessageUnsupportedSyntax SpreadArgument -> [text "A spread argument is unsupported here."]
    | MessageUnsupportedSyntax (UserDefinedTypeGuards { kind }) ->
      let kind_str =
        match kind with
        | Flow_ast.Type.TypeGuard.Default -> "This kind of type guard is"
        | Flow_ast.Type.TypeGuard.Asserts -> "Type guard assertions are"
        | Flow_ast.Type.TypeGuard.Implies -> "One-sided type guards are"
      in
      [text (kind_str ^ " not yet supported.")]
    | MessageUnsupportedSyntax (UnsupportedInternalSlot { name; static = false }) ->
      [text "Unsupported internal slot "; code name; text "."]
    | MessageUnsupportedSyntax (UnsupportedInternalSlot { name; static = true }) ->
      [text "Unsupported static internal slot "; code name; text "."]
    | MessageUnsupportedSyntax WithStatement ->
      [text "Flow doesn't support "; code "with"; text " statements."]
    | MessageUnsupportedVarianceAnnotation kind ->
      [text "Variance modifiers cannot appear on a type parameter of a "; text kind; text "."]
    | MessageUnsupportedSyntax NonnullAssertion ->
      [text "Flow doesn't support non-null assertions (the postfix "; code "!"; text " operator)."]
    | MessageUntypedImport module_name ->
      [
        text "Importing from an untyped module makes it ";
        code "any";
        text " ";
        text "and is not safe! Did you mean to add ";
        code "// @flow";
        text " ";
        text "to the top of ";
        code module_name;
        text "?";
      ]
    | MessageUntypedTypeImport module_name ->
      [
        text "Importing a type from an untyped module makes it ";
        code "any";
        text " ";
        text "and is not safe! Did you mean to add ";
        code "// @flow";
        text " to ";
        text "the top of ";
        code module_name;
        text "?";
      ]
    | MessageUnusedPromiseInAsyncScope ->
      [
        code "Promise"; text " in async scope is unused. Did you mean to "; code "await"; text " it?";
      ]
    | MessageUnusedPromiseInSyncScope ->
      [
        code "Promise";
        text
          " in sync scope is unused. Promises must be handled by calling .then with a rejection handler, .catch, or .finally.";
      ]
    | MessageUnusedSuppression -> [text "Unused suppression comment."]
    | MessageValueUsedAsType description ->
      [
        text "Cannot use ";
        desc description;
        text " as a type. ";
        text "A name can be used as a type only if it refers to ";
        text "a type, interface, class, or enum definition. ";
        text "To get the type of a non-class value, use ";
        code "typeof";
        text ".";
      ]
    | MessageVariableNeverInitAssignedAnnotated reason ->
      [text "Variable "; ref reason; text " is never initialized, annotated, or assigned to."]
    | MessageVariableOnlyAssignedByNull { reason; null_loc } ->
      let null_ref =
        match null_loc with
        | Some loc -> ref (mk_reason (RCode "null") loc)
        | None -> code "null"
      in
      [
        text "Variable ";
        ref reason;
        text " is only ever assigned to by ";
        null_ref;
        text ". This is likely unintended; if it is intended, annotate ";
        desc (desc_of_reason reason);
        text " with ";
        code ": null";
        text " to disambiguate.";
      ]
    | MessageMatchNotExhaustive { examples } ->
      let examples =
        Base.List.map examples ~f:(fun (pattern, reasons) ->
            let reasons =
              Base.List.map reasons ~f:(fun reason -> [ref reason])
              |> Flow_errors_utils.Friendly.conjunction_concat ~limit:3
            in
            [code pattern; text " to match "] @ reasons
        )
      in
      let examples =
        match examples with
        | [] -> []
        | [single_example] -> (text "missing pattern: " :: single_example) @ [text "."]
        | examples ->
          let size = Base.List.length examples in
          let (examples, suffix) =
            let limit = 6 in
            if size > limit then
              ( Base.List.take examples (limit - 1),
                [text (Utils_js.spf "\nand %d others." (size - (limit - 1)))]
              )
            else
              (examples, [])
          in
          let examples =
            examples
            |> Base.List.map ~f:(fun example -> text "- " :: example)
            |> Base.List.intersperse ~sep:[text "\n"]
            |> Base.List.concat
          in
          (text "missing patterns:\n" :: examples) @ suffix
      in
      [code "match"; text " hasn't checked all possible cases of the input type. To fix, add the "]
      @ examples
    | MessageMatchUnnecessaryPattern { reason; already_seen } ->
      let msg =
        match already_seen with
        | Some already_seen ->
          [
            text "The values it matches were already covered by a previous ";
            ref already_seen;
            text ". ";
          ]
        | None ->
          [
            text
              "The values it matches are either already covered by previous patterns, or are not part of the input type. ";
          ]
      in
      [text "This "; ref reason; text " is unused. "]
      @ msg
      @ [text "To fix, either remove this pattern or restructure previous patterns."]
    | MessageMatchNonExhaustiveObjectPattern { rest; missing_props; pattern_kind } ->
      let pattern_kind_str = MatchObjPatternKind.to_string pattern_kind in
      let prefix =
        [
          text "This ";
          text pattern_kind_str;
          text " hasn't considered all possible properties of the input. ";
        ]
      in
      let has_missing_props = not @@ Base.List.is_empty missing_props in
      let (properties_text, those_properties_text) =
        if Base.List.length missing_props > 1 then
          ("properties", "those properties")
        else
          ("property", "that property")
      in
      let missing_props =
        if has_missing_props then
          text (spf "The %s " properties_text)
          :: (Base.List.map missing_props ~f:(fun prop ->
                  let prop =
                    if Parser_flow.string_is_valid_identifier_name prop then
                      prop
                    else
                      Js_layout_generator.quote_string ~prefer_single_quotes:true prop
                  in
                  [code prop]
              )
             |> Flow_errors_utils.Friendly.conjunction_concat ~limit:5
             )
          @ [text " are missing from the pattern"]
        else
          []
      in
      let suffix =
        let msg =
          match rest with
          | Some reason ->
            let msg =
              [text "could be additional properties due to "; ref reason; text ". To fix, add "]
            in
            if has_missing_props then
              missing_props @ (text ", and there " :: msg)
            else
              text "There " :: msg
          | None ->
            missing_props
            @ [
                text
                  (spf
                     ". To fix, either add %s to the %s, or add "
                     those_properties_text
                     pattern_kind_str
                  );
              ]
        in
        msg @ [code "..."; text " to the end of the pattern to match all other properties."]
      in
      prefix @ suffix
    | MessageMatchNonExplicitEnumCheck { wildcard_reason; unchecked_members } ->
      let unchecked_members =
        Base.List.map unchecked_members ~f:(fun enum_member -> [code enum_member])
        |> Flow_errors_utils.Friendly.conjunction_concat ~limit:5
      in
      [text "The "; ref wildcard_reason; text " does not check for missing enum "]
      @ ( if Base.List.length unchecked_members > 1 then
          [text "members "]
        else
          [text "member "]
        )
      @ unchecked_members
      @ [
          text ", because the ";
          code (Lints.string_of_kind Lints.RequireExplicitEnumChecks);
          text " lint has been enabled. ";
          text "To fix, add cases covering those enum members.";
        ]
    | MessageMatchInvalidGuardedWildcard ->
      [
        text "Cannot have a wildcard which is guarded using an ";
        code "if";
        text " in the last case of a ";
        code "match";
        text ". ";
        text "Either reorder this case, add a final unguarded (no ";
        code "if";
        text ") wildcard ";
        code "_";
        text " in a case after this one, or remove the pattern.";
      ]
    | MessageMatchInvalidIdentOrMemberPattern { type_reason } ->
      [
        text "Cannot have ";
        ref type_reason;
        text " in a match pattern position. ";
        text "Valid types for match patterns include string literals, number literals, ";
        text "bigint literals, boolean literals, enum members, null, or undefined.";
      ]
    | MessageMatchInvalidBindingKind { kind } ->
      [
        text "Cannot use ";
        code (Flow_ast_utils.string_of_variable_kind kind);
        text " for match pattern binding. Only ";
        code "const";
        text " is allowed.";
      ]
    | MessageMatchInvalidObjectPropertyLiteral { pattern_kind } ->
      [
        text "Unsupported ";
        text (MatchObjPatternKind.to_string pattern_kind);
        text " property literal in match pattern. ";
        text "String literals and int-like number literals are supported.";
      ]
    | MessageMatchInvalidUnaryZero ->
      [text "Unary pattern on "; code "0"; text " is not supported."]
    | MessageMatchInvalidUnaryPlusBigInt ->
      [text "Unary pattern "; code "+"; text " on bigint literal is not supported."]
    | MessageMatchDuplicateObjectProperty { name; pattern_kind } ->
      [
        text "Duplicate property ";
        code name;
        text " in ";
        text (MatchObjPatternKind.to_string pattern_kind);
        text ".";
      ]
    | MessageMatchBindingInOrPattern ->
      [text "New bindings in 'or' patterns are not yet supported."]
    | MessageMatchInvalidAsPattern ->
      [text "Invalid "; code "as"; text " pattern. Direct use on a binding pattern is not allowed."]
    | MessageMatchInvalidPatternReference { binding_reason } ->
      [
        text "Can't use variable ";
        ref binding_reason;
        text " within the same match pattern it is defined.";
      ]
    | MessageMatchInvalidObjectShorthand { name; pattern_kind } ->
      [
        code "match";
        text " ";
        text (MatchObjPatternKind.to_string pattern_kind);
        text "s don't allow this property shorthand syntax. ";
        text "To fix, be explicit and either use ";
        code (spf "{const %s}" name);
        text " if you want to create a new variable with the value of property ";
        code name;
        text ", or use ";
        code (spf "{%s: %s}" name name);
        text " if you want to match property ";
        code name;
        text " against the value of the variable named ";
        code name;
        text ".";
      ]
    | MessageMatchStatementInvalidBody ->
      [
        code "match";
        text " statements use blocks for each case body. ";
        text "To fix, wrap this statement with ";
        code "{";
        text " and ";
        code "}";
        text ".";
      ]
    | MessageMatchInvalidCaseSyntax kind ->
      let msg_invalid_prefix_case =
        [text "Drop the "; code "case"; text ". Just <pattern>. It's cleaner."]
      in
      let msg_invalid_infix_colon =
        [
          code "match";
          text " cases use ";
          code "=>";
          text " to separate the pattern and the case body. To fix, replace the ";
          code ";";
          text " with ";
          code "=>";
          text ".";
        ]
      in
      let msg_invalid_suffix_semicolon =
        [
          code "match";
          text " uses commas ";
          code ",";
          text " to separate cases. To fix, replace the ";
          code ";";
          text " with ";
          code ",";
          text ".";
        ]
      in
      (match kind with
      | InvalidMatchCaseMultiple
          { invalid_prefix_case_locs; invalid_infix_colon_locs; invalid_suffix_semicolon_locs } ->
        let msg_with_locs msg locs =
          if Base.List.is_empty locs then
            None
          else
            let refs = Base.List.map locs ~f:(fun loc -> no_desc_ref loc) in
            Some (msg @ [text " At"] @ refs @ [text "."])
        in
        let errors =
          Base.List.filter_opt
            [
              msg_with_locs msg_invalid_prefix_case invalid_prefix_case_locs;
              msg_with_locs msg_invalid_infix_colon invalid_infix_colon_locs;
              msg_with_locs msg_invalid_suffix_semicolon invalid_suffix_semicolon_locs;
            ]
        in
        (match errors with
        | [single] -> single
        | multiple ->
          text "Invalid match case syntax:"
          :: Base.List.concat_map multiple ~f:(fun x -> text "\n- " :: x))
      | InvalidMatchCasePrefixCase -> msg_invalid_prefix_case
      | InvalidMatchCaseInfixColon -> msg_invalid_infix_colon
      | InvalidMatchCaseSuffixSemicolon -> msg_invalid_suffix_semicolon)
    | MessageMatchInvalidWildcardSyntax ->
      [
        code "match";
        text " uses ";
        code "_";
        text " for wildcard patterns which match everything. ";
        text "To fix, replace ";
        code "default";
        text " with ";
        code "_";
        text ".";
      ]
    | MessageMatchInvalidInstancePattern ->
      [text "Invalid match instance pattern constructor. It must reference a single class."]
    | MessageRecordBannedTypeUtil { reason_op; reason_record } ->
      [
        text "Operation ";
        ref reason_op;
        text " is not allowed on record ";
        ref reason_record;
        text ". ";
        text "To fix, turn the record type into an object type first using ";
        code "{...MyRecord}";
        text ", for example: ";
        code "TypeUtil<{...MyRecord}>";
        text ".";
      ]
    | MessageRecordInvalidNew { record_name } ->
      [
        text "Use a record expression directly instead of ";
        code "new";
        text " with an object literal. For example: ";
        code (spf "%s {...}" record_name);
        text ".";
      ]
    | MessageRecordInvalidName { name } ->
      [
        text "Invalid record name ";
        code name;
        text ". Record names cannot start with lowercase 'a' through 'z'.";
      ]
    | MessageRecordDeclarationInvalidSyntax kind ->
      let msg_invalid_infix_equals =
        [text "Record declarations don't need the "; code "="; text ", remove it."]
      in
      let msg_invalid_variance =
        [
          text "Record declaration properties are read-only by default, ";
          text "so don't allow variance annotations. Remove to fix.";
        ]
      in
      let msg_invalid_optional =
        [
          text "Record declaration properties can't be optional. ";
          text "Instead, you can specify a default value for the property using ";
          code "= <expression>,";
          text " after the type annotation. ";
          text "This default value will be used if the property ";
          text "is omitted when creating the record. ";
          text "To closely mimic the existing behavior of an optional property, ";
          text "you can change ";
          code "foo?: T,";
          text " to ";
          code "foo: T | void = undefined,";
          text ", though there is likely a better default value you could use instead.";
        ]
      in
      let msg_invalid_suffix_semicolon =
        [
          text "Record declarations use commas ";
          code ",";
          text " to separate properties. ";
          text "To fix, replace the ";
          code ";";
          text " with ";
          code ",";
          text ".";
        ]
      in
      (match kind with
      | InvalidRecordDeclarationSyntaxMultiple
          {
            invalid_infix_equals_loc;
            invalid_variance_locs;
            invalid_optional_locs;
            invalid_suffix_semicolon_locs;
          } ->
        let msg_with_locs msg locs =
          if Base.List.is_empty locs then
            None
          else
            let refs = Base.List.map locs ~f:(fun loc -> no_desc_ref loc) in
            Some (msg @ [text " At"] @ refs @ [text "."])
        in
        let msg_with_loc_opt msg loc_opt =
          match loc_opt with
          | None -> None
          | Some loc -> Some (msg @ [text " At "; no_desc_ref loc; text "."])
        in
        let errors =
          Base.List.filter_opt
            [
              msg_with_loc_opt msg_invalid_infix_equals invalid_infix_equals_loc;
              msg_with_locs msg_invalid_variance invalid_variance_locs;
              msg_with_locs msg_invalid_optional invalid_optional_locs;
              msg_with_locs msg_invalid_suffix_semicolon invalid_suffix_semicolon_locs;
            ]
        in
        (match errors with
        | [single] -> single
        | multiple ->
          text "Invalid record declaration syntax:"
          :: Base.List.concat_map multiple ~f:(fun x -> text "\n- " :: x))
      | InvalidRecordDeclarationSyntaxVariance -> msg_invalid_variance
      | InvalidRecordDeclarationSyntaxOptional -> msg_invalid_optional
      | InvalidRecordDeclarationSyntaxSuffixSemicolon -> msg_invalid_suffix_semicolon
      | InvalidRecordDeclarationSyntaxInfixEquals -> msg_invalid_infix_equals)
    | MessageIncompatiblETypeParamConstIncompatibility { lower; upper } ->
      [
        text "type parameters ";
        ref lower;
        text " and ";
        ref upper;
        text " do not have matching const-modifier values";
      ]
    | MessageTypeParamConstInvalidPosition reason ->
      [
        text "Type parameter ";
        ref reason;
        text " cannot be declared as 'const'. ";
        text "'const' modifier can only appear on a function or method type parameter.";
      ]
    | MessageConstantCondition { is_truthy; show_warning; constant_condition_kind; reason } ->
      let base_message =
        [
          text
            (spf
               "This condition is%s %s."
               ( if show_warning then
                 " likely"
               else
                 ""
               )
               ( if is_truthy then
                 "truthy"
               else
                 "falsy"
               )
            );
        ]
      in
      let help_message =
        match constant_condition_kind with
        | ConstCond_General -> []
        | UnawaitedPromise -> [text " Perhaps you meant to use `await` to resolve the promise?"]
        | UncalledFunction -> [text " Perhaps you meant to call the function?"]
      in
      let warning_message =
        let suggested_loc =
          match reason with
          | Some reason ->
            (match annot_loc_of_reason reason with
            | Some loc -> Some loc
            | None -> def_loc_opt_of_reason reason)
          | None -> None
        in
        if show_warning then
          [
            text "\n[WARNING]: Flow's type inference may be incorrect that it could be null ";
            text "at runtime (due to `any` annotations, out-of-bounds array accesses, etc.). ";
            text "If the check is valid, you might want to make";
            (match suggested_loc with
            | Some loc -> no_desc_ref loc
            | None -> text " the type of this expression");
            text " nullable (`T` -> `?T`).";
          ]
        else
          []
      in
      base_message @ help_message @ warning_message
  in

  let rec convert_error_message
      { kind; loc; error_code; root; message; misplaced_source_file = _; unsuppressable = _ } =
    let root =
      Base.Option.map root ~f:(fun (loc, msg) ->
          let (kind, msg) = root_msg_to_root_kind_and_friendly_msgs msg in
          (loc, kind, msg)
      )
    in
    match message with
    | SingletonMessage { message; frames; explanations } ->
      let frames =
        Option.map (List.map (frame_to_friendly_msgs ~include_incompatibility_pair:true)) frames
      in
      let explanations = Option.map (List.map explanation_to_friendly_msgs) explanations in
      mk_error ~kind ?root ?frames ?explanations loc error_code (msg_to_friendly_msgs message)
    | SpeculationMessage { frames; explanations; branches } ->
      mk_speculation_error
        ~kind
        ~loc
        ~root
        ~frames:
          (List.map
             (fun f -> snd (frame_to_friendly_msgs ~include_incompatibility_pair:false f))
             frames
          )
        ~explanations:(List.map explanation_to_friendly_msgs explanations)
        ~error_code
        (Base.List.map branches ~f:(fun (i, e) -> (i, convert_error_message e)))
  in
  let printable_error = convert_error_message intermediate_error in
  let printable_error =
    if intermediate_error.unsuppressable then
      patch_unsuppressable_error printable_error
    else
      printable_error
  in
  let printable_error =
    match intermediate_error.misplaced_source_file with
    | None -> printable_error
    | Some source_file -> patch_misplaced_error ~strip_root source_file printable_error
  in
  printable_error

let make_errors_printable ~loc_of_aloc ~get_ast ~strip_root errors =
  let f err acc =
    let err =
      err
      |> make_intermediate_error ~loc_of_aloc
      |> to_printable_error ~loc_of_aloc ~get_ast ~strip_root
    in
    Flow_errors_utils.ConcreteLocPrintableErrorSet.add err acc
  in
  Flow_error.ErrorSet.fold f errors Flow_errors_utils.ConcreteLocPrintableErrorSet.empty
