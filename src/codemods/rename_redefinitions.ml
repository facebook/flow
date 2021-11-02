(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LMap = Loc_collections.LocMap
module LSet = Loc_collections.LocSet
module Scope_api = Scope_api.With_ALoc
module Ssa_api = Ssa_api.With_ALoc
module Provider_api = Provider_api.ALocProviders
open Loc_collections
open Insert_type_utils
open Utils_js

module Stats = struct
  type t = {
    num_total_errors: int;
    num_renamed_vars: int;
    num_name_collisions: int;
  }

  let empty = { num_total_errors = 0; num_renamed_vars = 0; num_name_collisions = 0 }

  let combine c1 c2 =
    {
      num_total_errors = c1.num_total_errors + c2.num_total_errors;
      num_renamed_vars = c1.num_renamed_vars + c2.num_renamed_vars;
      num_name_collisions = c1.num_name_collisions + c2.num_name_collisions;
    }

  let serialize s =
    let open Utils_js in
    [
      spf "total_errors: %d" s.num_total_errors;
      spf "renamable_vars: %d" s.num_renamed_vars;
      spf "name_collisions: %d" s.num_name_collisions;
    ]

  let report s =
    [
      string_of_row ~indent:2 "Number of vars with write errors" s.num_total_errors;
      string_of_row ~indent:2 "Number of renamable vars" s.num_renamed_vars;
      string_of_row ~indent:2 "Number of $Temps" s.num_name_collisions;
    ]
end

module Acc = Insert_type_utils.UntypedAcc (Stats)

type reduce_acc = {
  name_map: (string * (Loc.t, Loc.t) Ast.Expression.t') ALocMap.t;
  num_errors: int;
}

let empty_reduce_acc = { name_map = ALocMap.empty; num_errors = 0 }

(* This pass does not transform the AST. Instead, it determines what assignments can later be extracted into
   separate variables. It produces a map from locations of renamable assignments to the name of the variable and
   the RHS of the assignment AST node (used in figuring out exactly what to rename it to), as well as a set of all variable
   names in the program to ensure that there's no name collisions.
*)
let reduce cx ast =
  let errors = Context.errors cx in
  let loc_errors =
    Flow_error.ErrorSet.fold
      (fun error ->
        LSet.fold
          (fun loc -> LSet.add loc)
          (Codemod_constrained_write_utils.declaration_locs_of_constrained_write_error cx error))
      errors
      LSet.empty
  in
  let is_extractable_assignment =
    Codemod_constrained_write_utils.is_extractable_assignment cx loc_errors
  in
  if LSet.is_empty loc_errors then
    (SSet.empty, empty_reduce_acc)
  else
    let names = ref SSet.empty in
    let reduce_acc = ref { empty_reduce_acc with num_errors = LSet.cardinal loc_errors } in
    let reducer =
      object (this)
        inherit Codemod_annotator.Queries.ident_visitor ~init:names as super

        method add_renaming loc s (_, expr) =
          let ({ name_map; _ } as acc_deref) = !reduce_acc in
          reduce_acc :=
            {
              acc_deref with
              name_map =
                ALocMap.add
                  ~combine:(fun e1 e2 ->
                    if e1 = e2 then
                      e1
                    else
                      failwith "Different bindings for same location")
                  loc
                  (s, expr)
                  name_map;
            }

        method! statement s =
          match s with
          | ( _,
              Ast.Statement.Expression
                { Ast.Statement.Expression.expression = (loc, Ast.Expression.Assignment assign); _ }
            ) ->
            this#assignment_statement loc assign;
            s
          | _ -> super#statement s

        method assignment_statement
            loc ({ Ast.Expression.Assignment.operator; left; right; _ } as expr) =
          let _ = super#assignment loc expr in
          match (operator, left) with
          | ( None,
              ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (id_loc, { Ast.Identifier.name; _ }); _ }
              )
            ) ->
            let id_loc_aloc = ALoc.of_loc id_loc in
            if is_extractable_assignment id_loc_aloc then this#add_renaming id_loc_aloc name right
          | _ -> ()
      end
    in
    let _ = reducer#program ast in
    (!names, !reduce_acc)

module Naming = struct
  (* Special cases for function calls, rename `x = createFooContainer(x)` to `const xFoo = createFooContainer(x)`, and similar *)
  let prepost =
    [("create", "container"); ("with", "context"); ("with", ""); ("createwith", ""); ("create", "")]

  (* Special cases for function calls, rename `x = create(x)` to `const xContainer = create(x)`, and similar *)
  let direct_map =
    SMap.of_list
      [
        ("connect", "Connect"); ("create", "Container"); ("connector", "Connector"); ("memo", "Memo");
      ]

  let mem_name =
    let open Ast.Identifier in
    let open Ast.Expression.Member in
    function
    | { property = PropertyIdentifier (_, { name; _ }); _ } -> String.capitalize_ascii name
    | _ -> "Mem"

  let call_suffix default expr =
    let open Ast.Identifier in
    let open Ast.Expression in
    let open Ast.Expression.OptionalMember in
    let call_name =
      match expr with
      | Identifier (_, { name; _ }) -> Some name
      | Member mem -> Some (mem_name mem)
      | OptionalMember { member; _ } -> Some (mem_name member)
      | _ -> None
    in
    match call_name with
    | None -> default
    | Some name when SMap.mem name direct_map -> SMap.find name direct_map
    | Some name ->
      begin
        match
          Base.List.find_map prepost ~f:(fun (pre, post) ->
              let re = Str.regexp_case_fold (spf "%s\\([a-zA-Z0-9_]+\\)%s" pre post) in
              if Str.string_match re name 0 then
                Some (Str.matched_group 1 name)
              else
                None
          )
        with
        | Some name -> String.capitalize_ascii name
        | None when String.length name < 20 -> String.capitalize_ascii name
        | None -> default
      end

  let rec suffix expr =
    let open Ast.Identifier in
    let open Ast.Expression in
    let open Ast.Literal in
    let open Ast.Expression.Logical in
    let open Ast.Expression.OptionalCall in
    let open Ast.Expression.OptionalMember in
    let open Ast.Expression.TypeCast in
    match expr with
    | Array _ -> "Arr"
    | ArrowFunction _ -> "Callback"
    | Assignment _ -> "Assign"
    | Binary _ -> "Op"
    | Call { Call.callee = (_, exp); _ } -> call_suffix "Call" exp
    | Class _ -> "Class"
    | Comprehension _ -> "Comp"
    | Conditional _ -> "Cond"
    | Function _ -> "Callback"
    | Generator _ -> "Gen"
    | Identifier (_, { name; _ }) -> String.capitalize_ascii name
    | Import _ -> "Imp"
    | JSXElement _ -> "Element"
    | JSXFragment _ -> "Fragment"
    | Literal { value = String _; _ } -> "Str"
    | Literal { value = BigInt _; _ } -> "BigInt"
    | Literal { value = RegExp _; _ } -> "RegExp"
    | Literal { raw; _ }
      when String.length raw < 10 && Str.string_match (Str.regexp "[a-zA-Z0-9_]+") raw 0 ->
      String.capitalize_ascii (Str.matched_group 0 raw)
    | Literal { value = Number _; _ } -> "Number"
    | Literal { value = Null; _ } -> "Null"
    | Literal { value = Boolean _; _ } -> "Bool"
    | Logical { operator = And; _ } -> "And"
    | Logical { operator = Or; _ } -> "Or"
    | Logical { operator = NullishCoalesce; _ } -> "Coalesced"
    | Member mem -> mem_name mem
    | MetaProperty _ -> "Meta"
    | New { New.callee = (_, exp); _ } -> call_suffix "New" exp
    | Object _ -> "Obj"
    | OptionalCall { call = { Call.callee = (_, exp); _ }; _ } -> call_suffix "Call" exp
    | OptionalMember { member; _ } -> mem_name member
    | Sequence _ -> "Seq"
    | Super _ -> "Super"
    | TaggedTemplate _ -> "Template"
    | TemplateLiteral _ -> "Template"
    | This _ -> "This"
    | TypeCast { expression = (_, expr); _ } -> suffix expr
    | Unary _ -> "Op"
    | Update _ -> "Updated"
    | Yield _ -> "Yielded"

  (* Figure out what to rename the extracted variables to. This is a bunch of coarse heuristics that can definitely be tuned and improved *)
  let mk_renamings map names =
    let counter = ref 0 in
    let next_temp () =
      let n = !counter in
      counter := n + 1;
      spf "$Temp%d" n
    in
    ALocMap.fold
      (fun loc (name, expr) (used_names, out_map, collisions) ->
        let new_name = name ^ suffix expr in
        let rec gen_final_name collisions name =
          (* This adds a $temp suffix to avoid name collisions. It's super conservative and doesn't consider
             safe cases like variables in separate scopes having the same name. *)
          if SSet.mem name used_names then
            (* Recurse just in case, via some nightmare coincidence, there already is
               a `x$Temp0` in the program. *)
            gen_final_name (collisions + 1) (name ^ next_temp ())
          else
            (collisions, name)
        in
        let (collisions, final_name) = gen_final_name collisions new_name in
        (SSet.add final_name used_names, ALocMap.add loc final_name out_map, collisions))
      map
      (names, ALocMap.empty, 0)
end

let mapper cctx =
  let cx = Codemod_context.Typed.context cctx in
  let { Loc_env.var_info = { Env_api.ssa_values; _ }; _ } = Context.environment cx in
  object (this)
    inherit [Acc.t] Codemod_ast_mapper.mapper "Rename variables" Acc.empty as super

    val mutable renaming_map = ALocMap.empty (* Not really mutable--set in method `program` *)

    method! program ast =
      let file = cctx.Codemod_context.Typed.file in
      let (names, { name_map; num_errors }) = reduce cx ast in
      let (_, map, collisions) = Naming.mk_renamings name_map names in
      renaming_map <- map;
      if ALocMap.is_empty renaming_map then
        ast
      else
        let ast' = super#program ast in
        ( if ast != ast' then
          let extra =
            Stats.
              {
                num_total_errors = num_errors;
                num_renamed_vars = ALocMap.cardinal map;
                num_name_collisions = collisions;
              }
            
          in

          this#update_acc (fun acc ->
              Acc.update_stats
                { acc with Acc.changed_set = Utils_js.FilenameSet.add file acc.Acc.changed_set }
                extra
          )
        );
        ast'

    (* Skip keys, qualified identifiers *)
    method! object_key_identifier ident = ident

    method! typeof_member_identifier ident = ident

    method! member_property_identifier ident = ident

    method! pattern_object_property_identifier_key ?kind:_ id = id

    method! generic_qualified_identifier_type qual =
      let open Ast.Type.Generic.Identifier in
      let (loc, { qualification; id }) = qual in
      let qualification' = this#generic_identifier_type qualification in
      (* Skips the id part *)
      if qualification' == qualification then
        qual
      else
        (loc, { qualification = qualification'; id })

    method! pattern_identifier ?kind:_ id = id

    (* Figure out if a use of the variable at `loc` has been renamed, by seeing if
       it exactly reaches a renamed assignment and nothing else. *)
    method private renaming_of_use loc =
      let id_loc_aloc = ALoc.of_loc loc in
      let locs =
        ALocMap.find_opt id_loc_aloc ssa_values
        |> Base.Option.value_map
             ~f:
               (Base.List.filter_map ~f:(function
                   | Ssa_api.Uninitialized -> None
                   | Ssa_api.Write reason -> Some (Reason.aloc_of_reason reason)
                   )
                   )
             ~default:[]
      in
      match locs with
      | [write_loc] -> ALocMap.find_opt write_loc renaming_map
      | _ -> None

    method! jsx_element_name_identifier id =
      let open Ast.JSX.Identifier in
      let (loc, { name = _; comments }) = id in
      match this#renaming_of_use loc with
      | Some new_name -> (loc, { Ast.JSX.Identifier.name = new_name; comments })
      | None -> id

    method! identifier ((id_loc, { Ast.Identifier.name = _; comments }) as id) =
      match this#renaming_of_use id_loc with
      | Some new_name -> (id_loc, { Ast.Identifier.name = new_name; comments })
      | None -> id

    method! assignment loc expr =
      match super#assignment loc expr with
      | {
          Ast.Expression.Assignment.left =
            ( left_loc,
              Ast.Pattern.Identifier
                {
                  Ast.Pattern.Identifier.name =
                    (id_loc, { Ast.Identifier.name = _; comments = left_comments });
                  annot = left_annot;
                  optional = left_optional;
                }
            );
          _;
        } as assign
        when ALocMap.mem (ALoc.of_loc id_loc) renaming_map ->
        let new_name = ALocMap.find (ALoc.of_loc id_loc) renaming_map in
        let left =
          ( left_loc,
            Ast.Pattern.Identifier
              {
                Ast.Pattern.Identifier.name =
                  (* crazy const extraction hack, h/t Panos *)
                  (Loc.none, { Ast.Identifier.name = "const " ^ new_name; comments = left_comments });
                annot = left_annot;
                optional = left_optional;
              }
          )
        in
        { assign with Ast.Expression.Assignment.left }
      | expr -> expr
  end
