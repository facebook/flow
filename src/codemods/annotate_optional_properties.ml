(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LMap = Loc_collections.LocMap
module LSet = Loc_collections.LocSet
open Insert_type_utils
open Utils_js
open Reason

type prop_data = {
  obj_loc: Loc.t;
  init_locs: Loc.t list;
  prop_accesses: Reason.name list;
  name: Reason.name;
}

module PropDataSet = Flow_set.Make (struct
  type t = prop_data

  (* I am lazy and this is not perf critical *)
  let compare = compare
end)

let prop_accesses =
  Type.(
    fold_use_op
      (fun _ -> [])
      (fun acc ->
        let open Type in
        function
        | PropertyCompatibility { prop = Some name; _ } -> name :: acc
        | _ -> acc)
  )

let data_of_prop_missing_error error =
  let open Type in
  let open Error_message in
  let msg = Flow_error.msg_of_error error in
  let op = util_use_op_of_msg unknown_use (fun op _ -> op) msg in
  match (root_of_use_op op, msg) with
  | (SetProperty { value; _ }, EIncompatibleProp { reason_obj; prop = Some name; _ })
  | (SetProperty { value; _ }, EStrictLookupFailed { reason_obj; name = Some name; _ })
  | (SetProperty { value; _ }, EPropNotFound { reason_obj; prop_name = Some name; _ })
  | (GetProperty value, EIncompatibleProp { reason_obj; prop = Some name; _ })
  | (GetProperty value, EStrictLookupFailed { reason_obj; name = Some name; _ })
  | (GetProperty value, EPropNotFound { reason_obj; prop_name = Some name; _ })
  (* Destructuring *)
  | (UnknownUse, EStrictLookupFailed { reason_obj; reason_prop = value; name = Some name; _ }) ->
    let obj_loc = def_loc_of_reason reason_obj in
    let init_locs = [def_loc_of_reason value] in
    let prop_accesses = prop_accesses op in
    Some { obj_loc; name; init_locs; prop_accesses }
  | _ -> None

module ErrorStats = struct
  type t = { num_total_errors: int }

  let empty = { num_total_errors = 0 }

  let combine c1 c2 = { num_total_errors = c1.num_total_errors + c2.num_total_errors }

  let serialize s =
    let open Utils_js in
    [spf "total_errors: %d" s.num_total_errors]

  let report s = [string_of_row ~indent:2 "Number of errors" s.num_total_errors]
end

module Codemod_lti_annotator = Codemod_annotator.Make (ErrorStats)
module Acc = Insert_type_utils.Acc (ErrorStats)

let mapper ~preserve_literals ~max_type_size ~default_any (cctx : Codemod_context.Typed.t) =
  let lint_severities = Codemod_context.Typed.lint_severities cctx in
  let flowfixme_ast = Codemod_context.Typed.flowfixme_ast ~lint_severities cctx in
  object (this)
    inherit
      Codemod_lti_annotator.mapper
        ~default_any
        ~generalize_maybe:false
        ~generalize_react_mixed_element:false
        ~lint_severities
        ~max_type_size
        ~merge_arrays:false
        ~preserve_literals
        cctx
        () as super

    val mutable prop_data = PropDataSet.empty

    method private post_run () = ErrorStats.{ num_total_errors = PropDataSet.cardinal prop_data }

    method private get_annot ploc ty annot =
      let f loc _annot ty' = this#annotate_node loc ty' (fun a -> Ast.Type.Available a) in
      let error _ = Ast.Type.Available (Loc.none, flowfixme_ast) in
      this#opt_annotate ~f ~error ~expr:None ploc ty annot

    method private report_errors loc errors =
      Base.List.iter errors ~f:(fun e -> this#update_acc (fun acc -> Acc.error acc loc e));
      codemod_error_locs <- LSet.add loc codemod_error_locs

    method private dedup_props =
      Base.List.dedup_and_sort ~compare:(fun p1 p2 ->
          match (p1, p2) with
          | (Ty.NamedProp { name = name1; _ }, Ty.NamedProp { name = name2; _ }) ->
            compare name1 name2
          | _ -> compare p1 p2
      )

    method private get_props_for_obj oloc =
      let relevant_errors =
        PropDataSet.filter (fun { obj_loc; _ } -> Loc.equal obj_loc oloc) prop_data
      in
      let merged_errors =
        PropDataSet.fold
          (fun ({ name; _ } as data) acc ->
            match PropDataSet.find_first_opt (fun { name = name'; _ } -> name = name') acc with
            | Some ({ init_locs; _ } as previous) ->
              let data = { data with init_locs = data.init_locs @ init_locs } in
              PropDataSet.remove previous acc |> PropDataSet.add data
            | None -> PropDataSet.add data acc)
          relevant_errors
          PropDataSet.empty
      in
      PropDataSet.fold
        (fun { init_locs; name; prop_accesses; _ } acc ->
          let rec remove_anys = function
            | Ty.Any _ -> []
            | Ty.Union (_, t1, t2, ts) -> Base.List.bind ~f:remove_anys (t1 :: t2 :: ts)
            | t -> [t]
          in
          let rec access_prop accesses =
            Ty.(
              function
              | Obj { obj_props; _ } as t -> begin
                match accesses with
                | [] -> t
                | n :: rest ->
                  Base.List.find_map
                    ~f:(function
                      | NamedProp { name; prop = Field { t; _ }; _ } when name = n ->
                        Some (access_prop rest t)
                      | _ -> None)
                    obj_props
                  |> Base.Option.value ~default:(Ty.Any Ty.Untyped)
              end
              (* if we are object matching on a non-object, we should produce an any *)
              | _ when accesses <> [] -> Ty.Any Ty.Untyped
              | t -> t
            )
          in
          if display_string_of_name name = "" then
            acc
          else
            let t =
              let ts =
                Base.List.(
                  init_locs
                  >>= Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size
                      %> Result.to_list
                  >>= remove_anys
                  >>| access_prop prop_accesses
                )
              in
              match ts with
              | [] -> Some (Ty.Any Ty.Untyped)
              | [t] -> Some t
              | t :: rest -> Some (Ty.mk_union ~from_bounds:false ~flattened:true (t, rest))
            in
            Base.Option.value_map
              ~f:(fun t ->
                Ty.NamedProp
                  {
                    name;
                    inherited = false;
                    source = Ty.Other;
                    def_loc = None;
                    prop = Ty.Field { t; optional = true; polarity = Ty.Neutral };
                  }
                :: acc)
              ~default:acc
              t)
        merged_errors
        []

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let open Ast.Expression in
      let (loc, { id; init }) = decl in
      match init with
      (* var x = {}; *)
      | Some (oloc, Object _) ->
        let obj_props = this#get_props_for_obj oloc in
        if List.length obj_props = 0 then
          super#variable_declarator ~kind decl
        else
          let id =
            match id with
            | ( ploc,
                Ast.Pattern.Identifier
                  { annot = Ast.Type.Missing _ as annot; Ast.Pattern.Identifier.name; optional }
              ) ->
              (match
                 Codemod_annotator.get_validated_ty cctx ~preserve_literals ~max_type_size ploc
               with
              | Ok (Ty.Obj ty) ->
                let ty_obj =
                  { ty with Ty.obj_props = obj_props @ ty.Ty.obj_props |> this#dedup_props }
                in
                let annot' = this#get_annot ploc (Ok (Ty.Obj ty_obj)) annot in
                ( ploc,
                  Ast.Pattern.Identifier { annot = annot'; Ast.Pattern.Identifier.name; optional }
                )
              | Ok _ -> id
              | Error errs ->
                this#report_errors oloc errs;
                id)
            | _ -> id
          in
          (loc, { id; init })
      | _ -> super#variable_declarator ~kind decl

    method! program prog =
      let cx = Codemod_context.Typed.context cctx in
      let errors =
        Flow_error.ErrorSet.fold
          (fun error acc ->
            try
              Flow_error.ConcreteErrorSet.add
                (Flow_error.concretize_error
                   (ALoc.to_loc_with_tables (Context.aloc_tables cx))
                   error
                )
                acc
            with
            | Not_found -> acc)
          (Context.errors cx)
          Flow_error.ConcreteErrorSet.empty
      in
      let suppressions = Context.error_suppressions cx in
      prop_data <-
        Flow_error.ConcreteErrorSet.fold
          (fun error acc ->
            let errors =
              Flow_error.ConcreteErrorSet.singleton error |> Flow_error.make_errors_printable
            in
            let (errors, _, _) =
              Error_suppressions.filter_suppressed_errors
                ~root:Path.dummy_path
                ~file_options:None
                suppressions
                errors
                ~unused:suppressions
            in
            if Errors.ConcreteLocPrintableErrorSet.is_empty errors then
              acc
            else
              match data_of_prop_missing_error error with
              | Some data -> PropDataSet.add data acc
              | None -> acc)
          errors
          PropDataSet.empty;
      if PropDataSet.is_empty prop_data then
        prog
      else
        super#program prog
  end
