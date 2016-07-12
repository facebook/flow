(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Type
open Utils_js
open Reason

(** we keep a stack of reasons representing the operations
    taking place when flows are performed. the top op reason
    is used in messages for errors that take place during its
    residence.
  *)
module Ops : sig
  val clear : unit -> reason list
  val push : reason -> unit
  val pop : unit -> unit
  val peek : unit -> reason option
  val get : unit -> reason list
  val set : reason list -> unit
end = struct
  let ops = ref []
  let clear () = let orig = !ops in ops := []; orig
  let push r = ops := r :: !ops
  let pop () = ops := List.tl !ops
  let peek () = match !ops with r :: _ -> Some r | [] -> None
  let get () = !ops
  let set _ops = ops := _ops
end

(** error services for typecheck pipeline -
    API for building and adding errors to context during
    typechecking, subject to speculation state.
  *)

module Impl : sig

  (* speculative checking *)
  exception SpeculativeError of Errors.error

  (* Maintain a stack of speculative branches. See Speculation for the contents
     of the "branch" data structure.

     When speculating (i.e., when this stack is non-empty), some things are
     handled differently:

     (1) flow and unify actions on unresolved tvars are deferred
     (2) any errors cause short-cutting
  *)
  val set_speculative: Speculation.branch -> unit
  val restore_speculative: unit -> unit
  val speculating: unit -> bool

  (* decide whether an action should be deferred.
     when speculating, actions that involve unresolved tvars are deferred. *)
  val defer_action: Context.t -> Speculation.Action.t -> bool

  (* error info is a location followed by a list of strings.
     mk_info takes location and first string from a reason. *)
  val mk_info: reason -> string list -> Errors.info

  (* convert reason into error info *)
  val info_of_reason: reason -> Errors.info

  (* build warning from info and add to context *)
  val add_warning:
    Context.t -> ?extra:Errors.info_tree list -> Errors.info -> unit

  (* build error from info and add to context *)
  val add_error:
    Context.t -> ?extra:Errors.info_tree list -> Errors.info -> unit

  (* build error from info list and add to context *)
  val add_extended_error:
    Context.t -> ?extra:Errors.info_tree list -> Errors.info list -> unit

  (* build internal error from info and add to context *)
  val add_internal_error:
    Context.t -> ?extra:Errors.info_tree list -> Errors.info -> unit

  (* add typecheck (flow) error from message and LB/UB pair.
     note: reasons extracted from types may appear in either order *)
  val flow_err:
    Context.t -> Trace.t -> string -> ?extra:Errors.info_tree list ->
    Type.t -> Type.use_t ->
    unit

  (* for when a t has been extracted from a use_t *)
  val flow_err_use_t:
    Context.t -> Trace.t -> string -> ?extra:Errors.info_tree list ->
    Type.t -> Type.t ->
    unit

  (* add typecheck (flow) error from message and reason pair.
     reasons are not reordered *)
  val flow_err_reasons:
    Context.t -> Trace.t -> string -> ?extra:Errors.info_tree list ->
    reason * reason ->
    unit

  (* TODO remove once error messages are indexed *)
  val flow_err_prop_not_found:
    Context.t -> Trace.t -> reason * reason -> unit

  val warn_or_ignore_decorators:
    Context.t -> Loc.t -> unit

  val warn_or_ignore_class_properties:
    Context.t -> static:bool -> Loc.t -> unit

  val warn_unsafe_getters_setters:
    Context.t -> Loc.t -> unit

end = struct

  exception SpeculativeError of Errors.error

  let speculations = ref []
  let set_speculative branch =
    speculations := branch::!speculations
  let restore_speculative () =
    speculations := List.tl !speculations
  let speculating () = !speculations <> []

  let defer_action cx action =
    speculating() &&
      let branch = List.hd !speculations in
      Speculation.defer_if_relevant cx branch action

  let mk_info reason extra_msgs =
    loc_of_reason reason, desc_of_reason reason :: extra_msgs

  let info_of_reason r =
    mk_info r []

  (* lowish-level error logging.
     basic filtering and packaging before sending error to context. *)
  let add_output cx error =
    if speculating ()
    then raise (SpeculativeError error)
    else (
      if Context.is_verbose cx
      then prerr_endlinef "\nadd_output cx.file %S loc %s"
        (string_of_filename (Context.file cx))
        (string_of_loc (Errors.loc_of_error error));

      (* catch no-loc errors early, before they get into error map *)
      Errors.(
        if Loc.source (loc_of_error error) = None
        then assert_false (spf "add_output: no source for error: %s"
          (Hh_json.json_to_multiline (json_of_errors [error])))
      );

      Context.add_error cx error
    )

  let add_warning cx ?extra info =
    add_output cx Errors.(mk_error ~kind:InferWarning ?extra [info])

  let add_error cx ?extra info =
    add_output cx (Errors.mk_error ?extra [info])

  let add_extended_error cx ?extra infos =
    add_output cx (Errors.mk_error ?extra infos)

  let add_internal_error cx ?extra info =
    add_output cx Errors.(mk_error ~kind:InternalError ?extra [info])

  (** build typecheck error from msg, reasons, trace and extra info.
      Note: Ops stack is also queried, so this isn't a stateless function.
    *)
  let typecheck_error cx trace msg ?extra (r1, r2) =
    let origin_r1 = origin_of_reason r1 in
    let origin_infos = opt_map_default (fun r -> [mk_info r []]) [] origin_r1 in
    (* make core info from reasons, message, and optional extra infos *)
    let core_infos = [
      mk_info r1 [msg];
      mk_info r2 []
    ] in
    (* Since pointing to endpoints in the library without any information on
       the code that uses those endpoints inconsistently is useless, we point
       to the file containing that code instead. Ideally, improvements in
       error reporting would cause this case to never arise. *)
    let lib_infos = if is_lib_reason r1 && is_lib_reason r2 then
        let loc = Loc.({ none with source = Some (Context.file cx) }) in
        [loc, ["inconsistent use of library definitions"]]
      else []
    in
    (* trace info *)
    let trace_infos =
      (* format a trace into list of (reason, desc) pairs used
       downstream for obscure reasons, and then to messages *)
      let max_trace_depth = Context.max_trace_depth cx in
      if max_trace_depth = 0 then [] else
        let strip_root = Context.should_strip_root cx in
        let root = Context.root cx in
        let prep_path r =
          if not strip_root then r
          else Reason.strip_root root r
        in
        Trace.reasons_of_trace ~prep_path ~level:max_trace_depth trace
        |> List.map info_of_reason
    in
    (* NOTE: We include the operation's reason in the error message, unless it
       overlaps *both* endpoints, or r1's origin. *)
    let op_info = match Ops.peek () with
      | Some r when not (reasons_overlap r r1 && reasons_overlap r r2) -> begin
          match origin_r1 with
          | Some or1 when reasons_overlap r or1 -> None
          | _ -> Some (info_of_reason r)
        end
      | _ -> None
    in
    (* main info is core info with optional lib line prepended, and optional
       extra info appended. ops/trace info is held separately in error *)
    let msg_infos = lib_infos @ origin_infos @ core_infos in
    Errors.mk_error ?op_info ~trace_infos ?extra msg_infos

  let flow_err_reasons cx trace msg ?extra (r1, r2) =
    add_output cx (typecheck_error cx trace msg ?extra (r1, r2))

  (* TODO remove once error messages are indexed *)
  let flow_err_prop_not_found cx trace (r1, r2) =
    flow_err_reasons cx trace "Property not found in" (r1, r2)

  let warn_or_ignore_decorators cx loc =
    match Context.esproposal_decorators cx with
    | Options.ESPROPOSAL_ENABLE -> failwith "Decorators cannot be enabled!"
    | Options.ESPROPOSAL_IGNORE -> ()
    | Options.ESPROPOSAL_WARN ->
      add_warning cx (loc, [
        "Experimental decorator usage";
        "Decorators are an early stage proposal that may change. " ^
          "Additionally, Flow does not account for the type implications " ^
          "of decorators at this time."
      ])

  let warn_or_ignore_class_properties cx ~static loc =
    let config_setting, reason_subject, config_key =
      if static then
        Context.esproposal_class_static_fields cx,
        "class static field",
        "class_static_fields"
      else
        Context.esproposal_class_instance_fields cx,
        "class instance field",
        "class_instance_fields"
    in
    match config_setting with
    | Options.ESPROPOSAL_ENABLE
    | Options.ESPROPOSAL_IGNORE -> ()
    | Options.ESPROPOSAL_WARN ->
      add_warning cx (loc, [
        spf "Experimental %s usage" reason_subject;
        spf
        ("%ss are an active early stage feature proposal that may change. " ^^
         "You may opt-in to using them anyway in Flow by putting " ^^
         "`esproposal.%s=enable` into the [options] section of your .flowconfig.")
        (String.capitalize reason_subject)
        config_key
      ])

  let warn_unsafe_getters_setters cx loc =
    if not (Context.enable_unsafe_getters_and_setters cx)
    then add_warning cx (loc, [
      "Potentially unsafe get/set usage";
      ("Getters and setters with side effects are potentially unsafe and " ^
      "disabled by default. You may opt-in to using them anyway by putting " ^
      "`unsafe.enable_getters_and_setters=true` into the [options] section " ^
      "of your .flowconfig.");
    ])

  (* decide reason order based on UB's flavor and blamability *)
  let ordered_reasons l u =
    let rl = reason_of_t l in
    let ru = reason_of_use_t u in
    if is_use u || (is_blamable_reason ru && not (is_blamable_reason rl))
    then ru, rl
    else rl, ru

  (* build a flow error from an LB/UB pair *)
  let flow_err cx trace msg ?extra lower upper =
    let r1, r2 = ordered_reasons lower upper in
    flow_err_reasons cx trace msg ?extra (r1, r2)

  (* for when a t has been extracted from a use_t *)
  let flow_err_use_t cx trace msg ?extra lower upper =
    flow_err cx trace msg ?extra lower (UseT (UnknownUse, upper))

end

include Impl
