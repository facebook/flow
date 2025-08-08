(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LSet = Loc_collections.LocSet
module ALSet = Loc_collections.ALocSet
module LMap = Loc_collections.LocMap
open Insert_type_utils

let compute_locs_to_change_error_code ~reader cx =
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let (errors, _warnings, suppressions) =
    Error_suppressions.filter_lints
      ~include_suppressions:(Context.include_suppressions cx)
      (Context.error_suppressions cx)
      (Context.errors cx)
      (Context.aloc_tables cx)
      (Context.severity_cover cx)
  in
  Error_suppressions.find_error_suppressions_to_change_error_code_for_codemod
    ~root:(Context.root cx)
    ~file_options:(Some (Context.file_options cx))
    ~unsuppressable_error_codes:SSet.empty
    ~loc_of_aloc
    ~suppressions
    ~errors

module Stats = struct
  type t = { changed: int }

  let empty = { changed = 0 }

  let combine c1 c2 = { changed = c1.changed + c2.changed }

  let serialize s =
    let open Utils_js in
    [spf "changed_error_code: %d" s.changed]

  let report s = [string_of_row ~indent:2 "Changed error code" s.changed]
end

module Acc = Acc (Stats)

let codes_to_change =
  [
    "incompatible-call";
    "incompatible-cast";
    "incompatible-extend";
    "incompatible-return";
    "incompatible-type-arg";
    "prop-missing";
  ]

let mapper (cctx : Codemod_context.Typed.t) =
  object (_this)
    inherit [Acc.t] Codemod_ast_mapper.mapper "" ~init:Acc.empty as super

    val mutable changed = 0

    val mutable locs = LSet.empty

    method private post_run () = { Stats.changed }

    method! comment (loc, comment) =
      let open Ast.Comment in
      if LSet.mem loc locs then (
        (* Due to comment attachment bugs, the comment might be attached more than once, which
         * can break printing of patches. This line de-duplicates the edits. *)
        locs <- LSet.remove loc locs;
        let text =
          Base.List.fold ~init:comment.text codes_to_change ~f:(fun text code ->
              Base.String.substr_replace_all ~pattern:code ~with_:"incompatible-type" text
          )
        in
        if text <> comment.text then changed <- changed + 1;
        (loc, { comment with text })
      ) else
        (loc, comment)

    method! program prog =
      locs <-
        compute_locs_to_change_error_code
          ~reader:cctx.Codemod_context.Typed.reader
          cctx.Codemod_context.Typed.cx;
      if LSet.is_empty locs then
        prog
      else
        super#program prog
  end
