(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Namespace_env

module SMap = Utils.SMap
module SSet = Utils.SSet

(* When dealing with an <?hh file, HHVM automatically imports a few
 * "core" classes into every namespace, mostly collections. Their
 * unqualified names always refer to this global version.
 *
 * Note that these are technically in the \HH namespace as far as the
 * runtime is concerned, but we treat them as in the global
 * namespace. This is a tiny bit weird, but since Facebook www all runs
 * in the global namespace relying on this autoimport, this makes the
 * most sense there.
 *
 * See hhvm/compiler/parser/parser.cpp Parser::getAutoAliasedClasses
 * for the canonical list of classes and Parser::onCall for the
 * canonical list of functions. *)
let autoimport_classes = [
  "Traversable";
  "KeyedTraversable";
  "Container";
  "KeyedContainer";
  "Iterator";
  "KeyedIterator";
  "Iterable";
  "KeyedIterable";
  "Collection";
  "Vector";
  "ImmVector";
  "Map";
  "ImmMap";
  "StableMap";
  "Set";
  "ImmSet";
  "Pair";
  "Awaitable";
  "AsyncIterator";
  "IMemoizeParam";
  "AsyncKeyedIterator";
  "InvariantException";
  "AsyncGenerator";
  "WaitHandle";
  "StaticWaitHandle";
  "WaitableWaitHandle";
  "ResumableWaitHandle";
  "AsyncFunctionWaitHandle";
  "AsyncGeneratorWaitHandle";
  "AwaitAllWaitHandle";
  "GenArrayWaitHandle";
  "GenMapWaitHandle";
  "GenVectorWaitHandle";
  "ConditionWaitHandle";
  "RescheduleWaitHandle";
  "SleepWaitHandle";
  "ExternalThreadEventWaitHandle"
]
let autoimport_funcs = [
  "invariant";
  "invariant_violation"
]

let autoimport_set =
  let autoimport_list = autoimport_classes @ autoimport_funcs in
  List.fold_left (fun s e -> SSet.add e s) SSet.empty autoimport_list
(* NOTE that the runtime is able to distinguish between class and
   function names when auto-importing *)
let is_autoimport_name id = SSet.mem id autoimport_set

let elaborate_into_current_ns nsenv id =
  match nsenv.ns_name with
    | None -> "\\" ^ id
    | Some ns -> "\\" ^ ns ^ "\\" ^ id

(* Resolves an identifier in a given namespace environment. For example, if we
 * are in the namespace "N\O", the identifier "P\Q" is resolved to "\N\O\P\Q".
 *
 * All identifiers are fully-qualified by this function; the internal
 * representation of identifiers inside the typechecker after naming is a fully
 * qualified identifier.
 *
 * It's extremely important that this function is idempotent. We actually
 * normalize identifiers in two phases. Right after parsing, we need to have
 * the class hierarchy normalized so that we can recompute dependencies for
 * incremental mode properly. Other identifiers are normalized during naming.
 * However, we don't do any bookkeeping to determine which we've normalized or
 * not, just relying on the idempotence of this function to make sure everything
 * works out. (Fully qualifying identifiers is of course idempotent, but there
 * used to be other schemes here.)
 *)
let elaborate_id_impl ~autoimport nsenv (p, id) =
  (* Go ahead and fully-qualify the name first. *)
  let fully_qualified =
    if id <> "" && id.[0] = '\\' then id
    else if autoimport && is_autoimport_name id then "\\" ^ id
    else begin
      (* Expand "use" imports. *)
      let bslash_loc =
        try String.index id '\\' with Not_found -> String.length id in
      let prefix = String.sub id 0 bslash_loc in
      if prefix = "namespace" && id <> "namespace" then begin
        (* Strip off the 'namespace\' (including the slash) from id, then
        elaborate back into the current namespace. *)
        let len = (String.length id) - bslash_loc  - 1 in
        elaborate_into_current_ns nsenv (String.sub id (bslash_loc + 1) len)
      end else match SMap.get prefix nsenv.ns_uses with
        | None -> elaborate_into_current_ns nsenv id
        | Some use -> begin
          (* Strip off the "use" from id, but *not* the backslash after that
           * (so "use\foo" will become "\foo") and then prepend the new
           * namespace. *)
          let len = (String.length id) - bslash_loc in
          use ^ (String.sub id bslash_loc len)
        end
    end in
  p, fully_qualified

let elaborate_id = elaborate_id_impl ~autoimport:true
(* When a name that clashes with an auto-imported name is first being
 * defined (in its own namespace), it's impressively incorrect to
 * teleport it's definition into the global namespace *)
let elaborate_id_no_autos = elaborate_id_impl ~autoimport:false

(* First pass of flattening namespaces, run super early in the pipeline, right
 * after parsing.
 *
 * Fully-qualifies the things we need for Parsing_service.AddDeps -- the classes
 * we extend, traits we use, interfaces we implement; along with classes we
 * define. So that we can also use them to figure out fallback behavior, we also
 * fully-qualify functions that we define, even though AddDeps doesn't need
 * them this early.
 *
 * Note that, since AddDeps doesn't need it, we don't recursively traverse
 * through Happly in hints -- we rely on the idempotence of elaborate_id to
 * allow us to fix those up during a second pass during naming.
 *)
module ElaborateDefs = struct
  let hint nsenv = function
    | p, Happly (id, args) ->
        p, Happly (elaborate_id nsenv id, args)
    | other -> other

  let class_def nsenv = function
    | ClassUse h -> ClassUse (hint nsenv h)
    | XhpAttrUse h -> XhpAttrUse (hint nsenv h)
    | other -> other

  let rec def nsenv = function
    (*
      The default namespace in php is the global namespace specified by
      the empty string. In the case of an empty string, we model it as
      the global namespace.
    *)
    | Namespace ((_, nsname), prog) -> begin
        let nsname = match nsname with
          | "" -> None
          | _ -> Some nsname in
        let new_nsenv = {nsenv with ns_name = nsname} in
        nsenv, program new_nsenv prog
      end
    | NamespaceUse l -> begin
        let map = List.fold_left begin fun map (id1, id2) ->
          SMap.add (snd id2) (snd id1) map
        end nsenv.ns_uses l in
        {nsenv with ns_uses = map}, []
      end
    | Class c -> nsenv, [Class {c with
        c_name = elaborate_id_no_autos nsenv c.c_name;
        c_extends = List.map (hint nsenv) c.c_extends;
        c_implements = List.map (hint nsenv) c.c_implements;
        c_body = List.map (class_def nsenv) c.c_body;
        c_namespace = nsenv;
      }]
    | Fun f -> nsenv, [Fun {f with
        f_name = elaborate_id_no_autos nsenv f.f_name;
        f_namespace = nsenv;
      }]
    | Typedef t -> nsenv, [Typedef {t with
        t_id = elaborate_id_no_autos nsenv t.t_id;
        t_namespace = nsenv;
    }]
    | Constant cst -> nsenv, [Constant {cst with
        cst_name = elaborate_id_no_autos nsenv cst.cst_name;
        cst_namespace = nsenv;
    }]
    | other -> nsenv, [other]

  and program nsenv p =
    let _, acc = List.fold_left begin fun (nsenv, acc) item ->
      let nsenv, item = def nsenv item in
      nsenv, item :: acc
    end (nsenv, []) p in
    List.concat (List.rev acc)
end

let elaborate_defs ast = ElaborateDefs.program empty ast
