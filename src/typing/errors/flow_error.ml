(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Utils_js
open Reason
open Error_message

exception ImproperlyFormattedError of Loc.t Error_message.t'

type 'loc t = {
  loc: 'loc option;
  msg: 'loc Error_message.t';
  source_file: File_key.t;
  trace_reasons: 'loc Reason.virtual_reason list;
}

let loc_of_error { loc; _ } = loc

let msg_of_error { msg; _ } = msg

let source_file { source_file; _ } = source_file

let trace_reasons { trace_reasons; _ } = trace_reasons

let map_loc_of_error f { loc; msg; source_file; trace_reasons } =
  {
    loc = Option.map ~f loc;
    msg = map_loc_of_error_message f msg;
    source_file;
    trace_reasons = Core_list.map ~f:(Reason.map_reason_locs f) trace_reasons;
  }

let concretize_error lazy_table_of_aloc =
  map_loc_of_error (fun aloc ->
      let table = lazy_table_of_aloc aloc in
      ALoc.to_loc table aloc)

let kind_of_error err = msg_of_error err |> kind_of_msg

(* I wish OCaml's scoping for types was better *)
type 'loc err = 'loc t

module Error (M : Set.OrderedType) : Set.OrderedType with type t = M.t err = struct
  type t = M.t err

  let compare = compare
end

module ErrorSet = Set.Make (Error (ALoc))
module ConcreteErrorSet = Set.Make (Error (Loc))

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
            ( acc
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
            | _ -> frame_score ))
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
    | EPropNotFound (_, _, Frame (PropertyCompatibility _, _))
    | EStrictLookupFailed (_, _, _, Some (Frame (PropertyCompatibility _, _))) ->
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
      | EIncompatibleWithUseOp (rl, ru, _)
      | EIncompatibleWithExact ((rl, ru), _) ->
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

(* Decide reason order based on UB's flavor and blamability.
   If the order is unchanged, maintain reference equality. *)
let ordered_reasons ((rl, ru) as reasons) =
  if is_blamable_reason ru && not (is_blamable_reason rl) then
    (ru, rl)
  else
    reasons

let error_of_msg ~trace_reasons ~source_file (msg : ALoc.t Error_message.t') : ALoc.t t =
  { loc = aloc_of_msg msg; msg; source_file; trace_reasons }

let rec make_error_printable lazy_table_of_aloc (error : Loc.t t) : Loc.t Errors.printable_error =
  Errors.(
    let {
      loc : Loc.t option;
      msg : Loc.t Error_message.t';
      source_file;
      trace_reasons : Loc.t virtual_reason list;
    } =
      error
    in
    let kind = kind_of_msg msg in
    let mk_info (reason : concrete_reason) extras =
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
      (loc_of_reason reason, desc :: extras)
    in
    let info_of_reason (r : concrete_reason) = mk_info r [] in
    let trace_infos = Core_list.map ~f:info_of_reason trace_reasons in
    (* Flip the lower/upper reasons of a frame_use_op. *)
    let flip_frame = function
      | ArrayElementCompatibility c ->
        ArrayElementCompatibility { lower = c.upper; upper = c.lower }
      | FunCompatibility c -> FunCompatibility { lower = c.upper; upper = c.lower }
      | FunParam c -> FunParam { c with lower = c.upper; upper = c.lower }
      | FunRestParam c -> FunRestParam { lower = c.upper; upper = c.lower }
      | FunReturn c -> FunReturn { lower = c.upper; upper = c.lower }
      | IndexerKeyCompatibility c -> IndexerKeyCompatibility { lower = c.upper; upper = c.lower }
      | PropertyCompatibility c ->
        PropertyCompatibility { c with lower = c.upper; upper = c.lower }
      | ReactConfigCheck -> ReactConfigCheck
      | TupleElementCompatibility c ->
        TupleElementCompatibility { c with lower = c.upper; upper = c.lower }
      | TypeArgCompatibility c -> TypeArgCompatibility { c with lower = c.upper; upper = c.lower }
      | ( CallFunCompatibility _ | TupleMapFunCompatibility _ | ObjMapFunCompatibility _
        | ObjMapiFunCompatibility _ | TypeParamBound _ | FunMissingArg _ | ImplicitTypeParam
        | ReactGetConfig _ | UnifyFlip ) as use_op ->
        use_op
    in
    (* Unification produces two errors. One for both sides. For example,
     * {p: number} ~> {p: string} errors on both number ~> string and
     * string ~> number. Showing both errors to our user is often redundant.
     * So we use this utility to flip the string ~> number case and produce an
     * error identical to one we've produced before. These two errors will be
     * deduped in our PrintableErrorSet. *)
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
    let text = Friendly.text in
    let code = Friendly.code in
    let ref = Friendly.ref in
    let desc = Friendly.ref ~loc:false in
    (* Unwrap a use_op for the friendly error format. Takes the smallest location
     * where we found the error and a use_op which we will unwrap. *)
    let unwrap_use_ops =
      Friendly.(
        let rec loop (loc : Loc.t) frames (use_op : Loc.t virtual_use_op) =
          let action =
            match use_op with
            | Op UnknownUse
            | Op (Internal _) ->
              `UnknownRoot false
            | Op (Type.Speculation _) -> `UnknownRoot true
            | Op (ObjectSpread { op }) -> `Root (op, None, [text "Cannot spread "; desc op])
            | Op (ObjectChain { op }) ->
              `Root (op, None, [text "Incorrect arguments passed to "; desc op])
            | Op (Addition { op; left; right }) ->
              `Root (op, None, [text "Cannot add "; desc left; text " and "; desc right])
            | Op (AssignVar { var; init }) ->
              `Root
                ( init,
                  None,
                  match var with
                  | Some var -> [text "Cannot assign "; desc init; text " to "; desc var]
                  | None -> [text "Cannot assign "; desc init; text " to variable"] )
            | Op (InitField { op; body }) ->
              `Root (op, None, [text "Cannot initialize "; desc op; text " with "; desc body])
            | Op (Cast { lower; upper }) ->
              `Root (lower, None, [text "Cannot cast "; desc lower; text " to "; desc upper])
            | Op (ClassExtendsCheck { extends; def; _ }) ->
              `Root (def, None, [text "Cannot extend "; ref extends; text " with "; desc def])
            | Op (ClassImplementsCheck { implements; def; _ }) ->
              `Root (def, None, [text "Cannot implement "; ref implements; text " with "; desc def])
            | Op (ClassOwnProtoCheck { prop; own_loc; proto_loc }) ->
              (match (own_loc, proto_loc) with
              | (None, None) -> `UnknownRoot true
              | (Some loc, None) ->
                let def = mk_reason (RProperty (Some prop)) loc in
                `Root (def, None, [text "Cannot shadow proto property"])
              | (None, Some loc) ->
                let def = mk_reason (RProperty (Some prop)) loc in
                `Root (def, None, [text "Cannot define shadowed proto property"])
              | (Some own_loc, Some proto_loc) ->
                let def = mk_reason (RProperty (Some prop)) own_loc in
                let proto = mk_reason (RIdentifier prop) proto_loc in
                `Root (def, None, [text "Cannot shadow proto property "; ref proto]))
            | Op (Coercion { from; target }) ->
              `Root (from, None, [text "Cannot coerce "; desc from; text " to "; desc target])
            | Op (FunCall { op; fn; _ }) -> `Root (op, Some fn, [text "Cannot call "; desc fn])
            | Op (FunCallMethod { op; fn; prop; _ }) ->
              `Root (op, Some prop, [text "Cannot call "; desc fn])
            | Frame
                ( FunParam _,
                  ( Op (Type.Speculation (Op (FunCall _ | FunCallMethod _ | JSXCreateElement _)))
                  as use_op ) ) ->
              `Next use_op
            | Frame
                ( FunParam { n; name; lower = lower'; _ },
                  Op (FunCall { args; fn; _ } | FunCallMethod { args; fn; _ }) ) ->
              let lower =
                if List.length args > n - 1 then
                  List.nth args (n - 1)
                else
                  lower'
              in
              let param =
                match name with
                | Some name -> code name
                | None -> text (spf "the %s parameter" (Utils_js.ordinal n))
              in
              `Root
                ( lower,
                  None,
                  [
                    text "Cannot call ";
                    desc fn;
                    text " with ";
                    desc lower;
                    text " bound to ";
                    param;
                  ] )
            | Op (FunReturnStatement { value }) ->
              `Root (value, None, [text "Cannot return "; desc value])
            | Op (FunImplicitReturn { upper; fn }) ->
              `Root
                ( upper,
                  None,
                  [text "Cannot expect "; desc upper; text " as the return type of "; desc fn] )
            | Op (GeneratorYield { value }) ->
              `Root (value, None, [text "Cannot yield "; desc value])
            | Op (GetProperty prop) -> `Root (prop, None, [text "Cannot get "; desc prop])
            | Frame (FunParam _, Op (JSXCreateElement { op; component; _ }))
            | Op (JSXCreateElement { op; component; _ }) ->
              `Root (op, Some component, [text "Cannot create "; desc component; text " element"])
            | Op (ReactCreateElementCall { op; component; _ }) ->
              `Root (op, Some component, [text "Cannot create "; desc component; text " element"])
            | Op (ReactGetIntrinsic { literal }) ->
              `Root (literal, None, [text "Cannot create "; desc literal; text " element"])
            | Op (TypeApplication { type' }) ->
              `Root (type', None, [text "Cannot instantiate "; desc type'])
            | Op (SetProperty { prop; value; lhs; _ }) ->
              let loc_reason =
                if Loc.contains (loc_of_reason lhs) loc then
                  lhs
                else
                  value
              in
              `Root (loc_reason, None, [text "Cannot assign "; desc value; text " to "; desc prop])
            | Frame (ArrayElementCompatibility { lower; _ }, use_op) ->
              `Frame (lower, use_op, [text "array element"])
            | Frame (FunParam { n; lower; _ }, use_op) ->
              `Frame (lower, use_op, [text "the "; text (Utils_js.ordinal n); text " argument"])
            | Frame (FunRestParam _, use_op) -> `Next use_op
            | Frame (FunReturn { lower; _ }, use_op) ->
              `Frame (repos_reason loc lower, use_op, [text "the return value"])
            | Frame (IndexerKeyCompatibility { lower; _ }, use_op) ->
              `Frame (lower, use_op, [text "the indexer property's key"])
            | Frame
                ( PropertyCompatibility { prop = None | Some "$key" | Some "$value"; lower; _ },
                  use_op ) ->
              `Frame (lower, use_op, [text "the indexer property"])
            | Frame (PropertyCompatibility { prop = Some "$call"; lower; _ }, use_op) ->
              `Frame (lower, use_op, [text "the callable signature"])
            | Frame (PropertyCompatibility { prop = Some prop; lower; _ }, use_op) ->
              let repos_small_reason loc reason = function
                (* If we are checking class extensions or implementations then the
                 * object reason will point to the class name. So don't reposition with
                 * this reason. *)
                | Op (ClassExtendsCheck _) -> repos_reason loc reason
                | Op (ClassImplementsCheck _) -> repos_reason loc reason
                | _ -> reason
              in
              let lower = repos_small_reason loc lower use_op in
              let rec loop lower = function
                (* Don't match $key/$value/$call properties since they have special
                 * meaning. As defined above. *)
                | Frame (PropertyCompatibility { prop = Some prop; lower = lower'; _ }, use_op)
                  when prop <> "$key" && prop <> "$value" && prop <> "$call" ->
                  let lower' = repos_small_reason (loc_of_reason lower) lower' use_op in
                  (* Perform the same frame location unwrapping as we do in our
                   * general code. *)
                  let lower =
                    if Loc.contains (loc_of_reason lower') (loc_of_reason lower) then
                      lower
                    else
                      lower'
                  in
                  let (lower, props, use_op) = loop lower use_op in
                  (lower, prop :: props, use_op)
                (* Perform standard iteration through these use_ops. *)
                | use_op -> (lower, [], use_op)
              in
              (* Loop through our parent use_op to get our property path. *)
              let (lower, props, use_op) = loop lower use_op in
              (* Create our final action. *)
              `Frame
                ( lower,
                  use_op,
                  [
                    text "property ";
                    code (List.fold_left (fun acc prop -> prop ^ "." ^ acc) prop props);
                  ] )
            | Frame (TupleElementCompatibility { n; lower; _ }, use_op) ->
              `Frame (lower, use_op, [text "index "; text (string_of_int (n - 1))])
            | Frame (TypeArgCompatibility { targ; lower; _ }, use_op) ->
              `Frame (lower, use_op, [text "type argument "; ref targ])
            | Frame (TypeParamBound { name }, use_op) ->
              `FrameWithoutLoc (use_op, [text "type argument "; code name])
            | Frame (FunCompatibility { lower; _ }, use_op) -> `NextWithLoc (lower, use_op)
            | Frame (FunMissingArg _, use_op)
            | Frame (ImplicitTypeParam, use_op)
            | Frame (ReactConfigCheck, use_op)
            | Frame (ReactGetConfig _, use_op)
            | Frame (UnifyFlip, use_op)
            | Frame (CallFunCompatibility _, use_op)
            | Frame (TupleMapFunCompatibility _, use_op)
            | Frame (ObjMapFunCompatibility _, use_op)
            | Frame (ObjMapiFunCompatibility _, use_op) ->
              `Next use_op
          in
          match action with
          (* Skip this use_op and go to the next one. *)
          | `Next use_op -> loop loc frames use_op
          (* Skip this use_op, don't add a frame, but do use the loc to reposition
           * our primary location. *)
          | `NextWithLoc (frame_reason, use_op) ->
            (* If our current loc is inside our frame_loc then use our current loc
             * since it is the smallest possible loc in our frame_loc. *)
            let frame_loc = loc_of_reason frame_reason in
            let loc =
              if Loc.contains frame_loc loc then
                loc
              else
                frame_loc
            in
            loop loc frames use_op
          (* Add our frame message and reposition the location if appropriate. *)
          | `Frame (frame_reason, use_op, frame) ->
            (* If our current loc is inside our frame_loc then use our current loc
             * since it is the smallest possible loc in our frame_loc. *)
            let frame_loc = loc_of_reason frame_reason in
            let frame_contains_loc = Loc.contains frame_loc loc in
            let loc =
              if frame_contains_loc then
                loc
              else
                frame_loc
            in
            (* Add our frame and recurse with the next use_op. *)
            let (all_frames, local_frames) = frames in
            let frames =
              ( frame :: all_frames,
                if frame_contains_loc then
                  local_frames
                else
                  frame :: local_frames )
            in
            loop loc frames use_op
          (* Same logic as `Frame except we don't have a frame location. *)
          | `FrameWithoutLoc (use_op, frame) ->
            let (all_frames, local_frames) = frames in
            let frames = (frame :: all_frames, frame :: local_frames) in
            loop loc frames use_op
          (* We don't know what our root is! Return what we do know. *)
          | `UnknownRoot show_all_frames ->
            let (all_frames, local_frames) = frames in
            ( None,
              loc,
              if show_all_frames then
                all_frames
              else
                local_frames )
          (* Finish up be returning our root location, root message, primary loc,
           * and frames. *)
          | `Root (root_reason, root_specific_reason, root_message) ->
            (* If our current loc is inside our root_loc then use our current loc
             * since it is the smallest possible loc in our root_loc. *)
            let root_loc = loc_of_reason root_reason in
            let root_specific_loc = Option.map root_specific_reason loc_of_reason in
            let loc =
              if Loc.contains root_loc loc && Loc.compare root_loc loc <> 0 then
                loc
              else
                Option.value root_specific_loc ~default:root_loc
            in
            (* Return our root loc and message in addition to the true primary loc
             * and frames. *)
            let (all_frames, _) = frames in
            (Some (root_loc, root_message), loc, all_frames)
        in
        fun (loc : Loc.t) (use_op : Loc.t virtual_use_op) ->
          let (root, loc, frames) = loop loc ([], []) use_op in
          let root =
            Option.map root (fun (root_loc, root_message) ->
                (root_loc, root_message @ [text " because"]))
          in
          (root, loc, frames))
    in
    (* Make a friendly error based on a use_op. The message we are provided should
     * not have any punctuation. Punctuation will be provided after the frames of
     * an error message. *)
    let mk_use_op_error (loc : Loc.t) (use_op : Loc.t virtual_use_op) message =
      let (root, loc, frames) = unwrap_use_ops loc use_op in
      mk_error ~trace_infos ?root ~frames loc message
    in
    (* Make a friendly error based on failed speculation. *)
    let mk_use_op_speculation_error (loc : Loc.t) (use_op : Loc.t virtual_use_op) branches =
      let (root, loc, frames) = unwrap_use_ops loc use_op in
      let speculation_errors =
        Core_list.map
          ~f:(fun (_, (msg : Error_message.t)) ->
            let score = score_of_msg msg in
            let error =
              error_of_msg ~trace_reasons:[] ~source_file msg
              |> concretize_error lazy_table_of_aloc
              |> make_error_printable lazy_table_of_aloc
            in
            (score, error))
          branches
      in
      mk_speculation_error ~kind:InferError ~trace_infos ~loc ~root ~frames ~speculation_errors
    in
    (* An error between two incompatible types. A "lower" type and an "upper"
     * type. The use_op describes the path which we followed to find
     * this incompatibility.
     *
     * This is a specialization of mk_incompatible_use_error. *)
    let mk_incompatible_error lower upper use_op =
      let ((lower, upper), use_op) = dedupe_by_flip (lower, upper) use_op in
      let ((lower, upper), use_op) = flip_contravariant (lower, upper) use_op in
      match use_op with
      (* Add a custom message for Coercion root_use_ops that does not include the
       * upper bound. *)
      | Op (Coercion { from; _ }) ->
        mk_use_op_error (loc_of_reason from) use_op [ref lower; text " should not be coerced"]
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
            [ref def; text " requires another argument"]
          | Frame (CallFunCompatibility { n }, _) ->
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
          | Frame (TupleMapFunCompatibility { value }, _) ->
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
          | Frame (ObjMapFunCompatibility { value }, _) ->
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
          | Frame (ObjMapiFunCompatibility { key; value }, _) ->
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
          | _ -> [ref def; text " requires another argument from "; ref op]
        in
        mk_use_op_error (loc_of_reason op) use_op message
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
          mk_use_op_error
            (loc_of_reason lower)
            use_op
            ( [ref lower; text " is incompatible with "]
            @
            if Loc.compare (loc_of_reason return) (loc_of_reason upper) = 0 then
              [text "implicitly-returned "; desc upper]
            else
              [ref upper] )
        (* Default incompatibility. *)
        | _ ->
          begin
            match (desc_of_reason lower, desc_of_reason upper) with
            | (RPolyTest _, RPolyTest _) when loc_of_reason lower = loc_of_reason upper ->
              mk_use_op_error
                (loc_of_reason lower)
                use_op
                [
                  text "the expected type is not parametric in ";
                  ref upper;
                  text ", perhaps due to the use of ";
                  code "*";
                  text " or the lack of a type annotation";
                ]
            | (RLongStringLit n, RStringLit _) ->
              mk_use_op_error
                (loc_of_reason lower)
                use_op
                [
                  ref lower;
                  text " is incompatible with ";
                  ref upper;
                  text " because strings longer than ";
                  code (string_of_int n);
                  text " characters are not treated as literals";
                ]
            | _ ->
              mk_use_op_error
                (loc_of_reason lower)
                use_op
                [ref lower; text " is incompatible with "; ref upper]
          end)
    in
    let mk_trust_incompatible_error lower upper use_op =
      match (desc_of_reason lower, desc_of_reason upper) with
      | ((RAnyExplicit | RAnyImplicit), (RTrusted _ | RPrivate (RTrusted _)))
      | ((RPrivate _ | RTrusted (RPrivate _)), (RAnyExplicit | RAnyImplicit)) ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          [ref lower; text " is incompatible with "; ref upper]
      | ((RAnyExplicit | RAnyImplicit), _) ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          [ref lower; text " is incompatible with trusted "; ref upper]
      | (_, (RAnyExplicit | RAnyImplicit)) ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          [text "private "; ref lower; text " is incompatible with "; ref upper]
      | (RPrivate _, RTrusted _) ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          ( [
              text "`any` may have been passed into ";
              ref lower;
              text " and `any` is incompatible with ";
              ref upper;
              text ", and ";
            ]
          @ [
              ref upper;
              text " may be passed into `any` and ";
              ref lower;
              text " is incompatible with `any`";
            ] )
      | (_, (RTrusted _ | RPrivate (RTrusted _))) ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          [
            text "`any` may have been passed into ";
            ref lower;
            text " and `any` is incompatible with ";
            ref upper;
          ]
      | ((RPrivate _ | RTrusted (RPrivate _)), _) ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          [
            ref upper;
            text " may be passed into `any` and ";
            ref lower;
            text " is incompatible with `any`";
          ]
      | _ ->
        mk_use_op_error
          (loc_of_reason lower)
          use_op
          [ref lower; text " is incompatible with "; ref upper]
    in
    (* When we fail to find a property on an object we use this function to create
     * an error. prop_loc should be the position of the use which caused this
     * error. The use_op represents how we got to this error.
     *
     * If the use_op is a PropertyCompatibility frame then we encountered this
     * error while subtyping two objects. In this case we add a bit more
     * information to the error message. *)
    let mk_prop_missing_error prop_loc prop lower use_op =
      let (loc, lower, upper, use_op) =
        match use_op with
        (* If we are missing a property while performing property compatibility
         * then we are subtyping. Record the upper reason. *)
        | Frame (PropertyCompatibility { prop = compat_prop; lower; upper; _ }, use_op)
          when prop = compat_prop ->
          (loc_of_reason lower, lower, Some upper, use_op)
        (* Otherwise this is a general property missing error. *)
        | _ -> (prop_loc, lower, None, use_op)
      in
      (* If we were subtyping that add to the error message so our user knows what
       * object required the missing property. *)
      let prop_message = mk_prop_message prop in
      let message =
        match upper with
        | Some upper ->
          prop_message @ [text " is missing in "; ref lower; text " but exists in "] @ [ref upper]
        | None -> prop_message @ [text " is missing in "; ref lower]
      in
      (* Finally, create our error message. *)
      mk_use_op_error loc use_op message
    in
    (* An error that occurs when some arbitrary "use" is incompatible with the
     * "lower" type. The use_op describes the path which we followed to find this
     * incompatibility.
     *
     * Similar to mk_incompatible_error except with any arbitrary *use*
     * instead of specifically an upper type. This error handles all use
     * incompatibilities in general. *)
    let mk_incompatible_use_error use_loc use_kind lower upper use_op =
      let nope msg = mk_use_op_error use_loc use_op [ref lower; text (" " ^ msg)] in
      match use_kind with
      | IncompatibleElemTOfArrT -> nope "is not an array index"
      | IncompatibleGetPrivatePropT
      | IncompatibleSetPrivatePropT ->
        nope "is not a class with private properties"
      | IncompatibleMixedCallT ->
        mk_use_op_error
          use_loc
          use_op
          [text "the parameter types of an "; ref lower; text " are unknown"]
      | IncompatibleCallT
      | IncompatibleConstructorT ->
        nope "is not a function"
      | IncompatibleObjAssignFromTSpread
      | IncompatibleArrRestT ->
        nope "is not an array"
      | IncompatibleObjAssignFromT
      | IncompatibleObjRestT
      | IncompatibleObjSealT
      | IncompatibleGetKeysT
      | IncompatibleGetValuesT ->
        nope "is not an object"
      | IncompatibleMapTypeTObject ->
        mk_use_op_error use_loc use_op [ref lower; text " is not a valid argument of "; ref upper]
      | IncompatibleMixinT
      | IncompatibleThisSpecializeT ->
        nope "is not a class"
      | IncompatibleSpecializeT
      | IncompatibleVarianceCheckT
      | IncompatibleTypeAppVarianceCheckT ->
        nope "is not a polymorphic type"
      | IncompatibleSuperT -> nope "is not inheritable"
      | IncompatibleUnaryMinusT -> nope "is not a number"
      | IncompatibleGetPropT (prop_loc, prop)
      | IncompatibleSetPropT (prop_loc, prop)
      | IncompatibleMatchPropT (prop_loc, prop)
      | IncompatibleHasOwnPropT (prop_loc, prop)
      | IncompatibleMethodT (prop_loc, prop) ->
        mk_prop_missing_error prop_loc prop lower use_op
      | IncompatibleGetElemT prop_loc
      | IncompatibleSetElemT prop_loc
      | IncompatibleCallElemT prop_loc ->
        mk_prop_missing_error prop_loc None lower use_op
      | IncompatibleGetStaticsT -> nope "is not an instance type"
      (* unreachable or unclassified use-types. until we have a mechanical way
       to verify that all legit use types are listed above, we can't afford
       to throw on a use type, so mark the error instead *)
      | IncompatibleUnclassified ctor -> nope (spf "is not supported by unclassified use %s" ctor)
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
        | Frame (PropertyCompatibility c, use_op) when c.prop = prop -> use_op
        | _ -> use_op
      in
      let expected =
        match lpole with
        | Polarity.Positive -> "read-only"
        | Polarity.Negative -> "write-only"
        | Polarity.Neutral ->
          (match upole with
          | Polarity.Negative -> "readable"
          | Polarity.Positive -> "writable"
          | Polarity.Neutral -> failwith "unreachable")
      in
      let actual =
        match upole with
        | Polarity.Positive -> "read-only"
        | Polarity.Negative -> "write-only"
        | Polarity.Neutral ->
          (match lpole with
          | Polarity.Negative -> "readable"
          | Polarity.Positive -> "writable"
          | Polarity.Neutral -> failwith "unreachable")
      in
      mk_use_op_error
        (loc_of_reason lower)
        use_op
        ( mk_prop_message prop
        @ [text (" is " ^ expected ^ " in "); ref lower; text " but "]
        @ [text (actual ^ " in "); ref upper] )
    in
    match (loc, friendly_message_of_msg msg) with
    | (Some loc, Error_message.Normal msg) -> mk_error ~trace_infos ~kind loc msg
    | (None, UseOp (loc, text, use_op)) -> mk_use_op_error loc use_op text
    | (None, PropMissing (prop_loc, prop, lower, use_op)) ->
      mk_prop_missing_error prop_loc prop lower use_op
    | (None, PropPolarityMismatch (x, p1, p2, use_op)) ->
      mk_prop_polarity_mismatch_error x p1 p2 use_op
    | (None, IncompatibleUse (loc, use_kind, lower, upper, use_op)) ->
      mk_incompatible_use_error loc use_kind lower upper use_op
    | (None, Incompatible (lower, upper, use_op)) -> mk_incompatible_error lower upper use_op
    | (None, IncompatibleTrust (lower, upper, use_op)) ->
      mk_trust_incompatible_error lower upper use_op
    | (None, Error_message.Speculation (loc, use_op, branches)) ->
      mk_use_op_speculation_error loc use_op branches
    | (None, Error_message.Normal _)
    | (Some _, _) ->
      raise (ImproperlyFormattedError msg))

let make_errors_printable lazy_table_of_aloc set =
  Errors.(
    ErrorSet.fold
      ( concretize_error lazy_table_of_aloc
      %> make_error_printable lazy_table_of_aloc
      %> ConcreteLocPrintableErrorSet.add )
      set
      ConcreteLocPrintableErrorSet.empty)
