(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast.Statement.EnumDeclaration

type 'M validation_error =
  | DuplicateMemberName of {
      loc: 'M;
      prev_use_loc: 'M;
      member_name: string;
    }
  | InconsistentMemberValues of { loc: 'M }
  | InvalidMemberInitializer of {
      loc: 'M;
      explicit_type: explicit_type option;
      member_name: string;
    }
  | BooleanMemberNotInitialized of {
      loc: 'M;
      member_name: string;
    }
  | NumberMemberNotInitialized of {
      loc: 'M;
      member_name: string;
    }
  | BigIntMemberNotInitialized of {
      loc: 'M;
      member_name: string;
    }
  | StringMemberInconsistentlyInitialized of { loc: 'M }
  | SymbolMemberWithInitializer of {
      loc: 'M;
      member_name: string;
    }
  | DuplicateMemberValue of {
      loc: 'M;
      prev_use_loc: 'M;
    }
  | InvalidMemberName of {
      loc: 'M;
      member_name: string;
    }
  | NonIdentifierMemberName of {
      loc: 'M;
      member_name: string;
    }

type 'M classified_members = {
  boolean_count: int;
  number_count: int;
  string_count: int;
  bigint_count: int;
  defaulted_count: int;
}

type 'M enum_rep =
  | BoolRep of bool option
  | NumberRep of { truthy: bool }
  | StringRep of { truthy: bool }
  | SymbolRep
  | BigIntRep of { truthy: bool }

type 'M member_info = {
  name: string;
  loc: 'M;
}

type 'M classified_result = {
  rep: 'M enum_rep;
  member_names: (string * 'M) list;
  has_unknown_members: 'M option;
}

let member_name (type a) (member : a member) : string =
  match member with
  | BooleanMember (_, { InitializedMember.id; _ })
  | NumberMember (_, { InitializedMember.id; _ })
  | StringMember (_, { InitializedMember.id; _ })
  | BigIntMember (_, { InitializedMember.id; _ })
  | DefaultedMember (_, { DefaultedMember.id }) ->
    Flow_ast_utils.string_of_enum_member_name id

let member_name_loc (type a) (id : a member_name) : a =
  match id with
  | Identifier (loc, _) -> loc
  | StringLiteral (loc, _) -> loc

let member_id_loc (type a) (member : a member) : a =
  match member with
  | BooleanMember (_, { InitializedMember.id; _ })
  | NumberMember (_, { InitializedMember.id; _ })
  | StringMember (_, { InitializedMember.id; _ })
  | BigIntMember (_, { InitializedMember.id; _ })
  | DefaultedMember (_, { DefaultedMember.id }) ->
    member_name_loc id

let member_loc (type a) (member : a member) : a =
  match member with
  | BooleanMember (loc, _)
  | NumberMember (loc, _)
  | StringMember (loc, _)
  | BigIntMember (loc, _)
  | DefaultedMember (loc, _) ->
    loc

let count_members members =
  List.fold_left
    (fun counts member ->
      match member with
      | BooleanMember _ -> { counts with boolean_count = counts.boolean_count + 1 }
      | NumberMember _ -> { counts with number_count = counts.number_count + 1 }
      | StringMember _ -> { counts with string_count = counts.string_count + 1 }
      | BigIntMember _ -> { counts with bigint_count = counts.bigint_count + 1 }
      | DefaultedMember _ -> { counts with defaulted_count = counts.defaulted_count + 1 })
    { boolean_count = 0; number_count = 0; string_count = 0; bigint_count = 0; defaulted_count = 0 }
    members

let check_duplicate_names members =
  let (_, errors) =
    List.fold_left
      (fun (seen, errs) member ->
        let name = member_name member in
        if name = "" then
          (seen, errs)
        else
          match SMap.find_opt name seen with
          | Some prev_use_loc ->
            let loc = member_id_loc member in
            (seen, DuplicateMemberName { loc; prev_use_loc; member_name = name } :: errs)
          | None ->
            let loc = member_id_loc member in
            (SMap.add name loc seen, errs))
      (SMap.empty, [])
      members
  in
  List.rev errors

let member_id (type a) (member : a member) : a member_name =
  match member with
  | BooleanMember (_, { InitializedMember.id; _ })
  | NumberMember (_, { InitializedMember.id; _ })
  | StringMember (_, { InitializedMember.id; _ })
  | BigIntMember (_, { InitializedMember.id; _ })
  | DefaultedMember (_, { DefaultedMember.id }) ->
    id

let check_member_names members =
  let is_a_to_z c = c >= 'a' && c <= 'z' in
  List.fold_left
    (fun errs member ->
      match member_id member with
      | Identifier _ ->
        let name = member_name member in
        if String.length name > 0 && is_a_to_z name.[0] then
          let loc = member_id_loc member in
          InvalidMemberName { loc; member_name = name } :: errs
        else
          errs
      | StringLiteral _ -> errs)
    []
    members
  |> List.rev

let check_non_identifier_member_names members =
  List.fold_left
    (fun errs member ->
      match member_id member with
      | StringLiteral (loc, { Flow_ast.StringLiteral.raw; _ }) ->
        NonIdentifierMemberName { loc; member_name = raw } :: errs
      | Identifier _ -> errs)
    []
    members
  |> List.rev

let check_duplicate_values members =
  let module BoolMap = Map.Make (Bool) in
  let module FloatMap = Map.Make (Float) in
  let module BigIntOptionMap = Map.Make (struct
    type t = int64 option

    let compare = Base.Option.compare Int64.compare
  end) in
  let (_, errors) =
    List.fold_left
      (fun ((bool_seen, num_seen, str_seen, bigint_seen), errs) member ->
        match member with
        | BooleanMember
            (_, { InitializedMember.init = (init_loc, { Flow_ast.BooleanLiteral.value; _ }); _ }) ->
          (match BoolMap.find_opt value bool_seen with
          | Some prev_use_loc ->
            ( (bool_seen, num_seen, str_seen, bigint_seen),
              DuplicateMemberValue { loc = init_loc; prev_use_loc } :: errs
            )
          | None ->
            let loc = member_loc member in
            ((BoolMap.add value loc bool_seen, num_seen, str_seen, bigint_seen), errs))
        | NumberMember
            (_, { InitializedMember.init = (init_loc, { Flow_ast.NumberLiteral.value; _ }); _ }) ->
          (match FloatMap.find_opt value num_seen with
          | Some prev_use_loc ->
            ( (bool_seen, num_seen, str_seen, bigint_seen),
              DuplicateMemberValue { loc = init_loc; prev_use_loc } :: errs
            )
          | None ->
            let loc = member_loc member in
            ((bool_seen, FloatMap.add value loc num_seen, str_seen, bigint_seen), errs))
        | StringMember
            (_, { InitializedMember.init = (init_loc, { Flow_ast.StringLiteral.value; _ }); _ }) ->
          (match SMap.find_opt value str_seen with
          | Some prev_use_loc ->
            ( (bool_seen, num_seen, str_seen, bigint_seen),
              DuplicateMemberValue { loc = init_loc; prev_use_loc } :: errs
            )
          | None ->
            let loc = member_loc member in
            ((bool_seen, num_seen, SMap.add value loc str_seen, bigint_seen), errs))
        | BigIntMember
            (_, { InitializedMember.init = (init_loc, { Flow_ast.BigIntLiteral.value; _ }); _ }) ->
          (match BigIntOptionMap.find_opt value bigint_seen with
          | Some prev_use_loc ->
            ( (bool_seen, num_seen, str_seen, bigint_seen),
              DuplicateMemberValue { loc = init_loc; prev_use_loc } :: errs
            )
          | None ->
            let loc = member_loc member in
            ((bool_seen, num_seen, str_seen, BigIntOptionMap.add value loc bigint_seen), errs))
        | DefaultedMember _ -> ((bool_seen, num_seen, str_seen, bigint_seen), errs))
      ((BoolMap.empty, FloatMap.empty, SMap.empty, BigIntOptionMap.empty), [])
      members
  in
  List.rev errors

let validate_explicit_boolean members =
  let errors = ref [] in
  List.iter
    (fun member ->
      match member with
      | BooleanMember _ -> ()
      | DefaultedMember (loc, { DefaultedMember.id }) ->
        errors :=
          BooleanMemberNotInitialized
            { loc; member_name = Flow_ast_utils.string_of_enum_member_name id }
          :: !errors
      | _ ->
        let loc = member_loc member in
        let name = member_name member in
        errors :=
          InvalidMemberInitializer { loc; explicit_type = Some Boolean; member_name = name }
          :: !errors)
    members;
  List.rev !errors

let validate_explicit_number members =
  let errors = ref [] in
  List.iter
    (fun member ->
      match member with
      | NumberMember _ -> ()
      | DefaultedMember (loc, { DefaultedMember.id }) ->
        errors :=
          NumberMemberNotInitialized
            { loc; member_name = Flow_ast_utils.string_of_enum_member_name id }
          :: !errors
      | _ ->
        let loc = member_loc member in
        let name = member_name member in
        errors :=
          InvalidMemberInitializer { loc; explicit_type = Some Number; member_name = name }
          :: !errors)
    members;
  List.rev !errors

let validate_explicit_string members =
  let errors = ref [] in
  let string_count =
    List.fold_left
      (fun count member ->
        match member with
        | StringMember _ -> count + 1
        | _ -> count)
      0
      members
  in
  let defaulted_count =
    List.fold_left
      (fun count member ->
        match member with
        | DefaultedMember _ -> count + 1
        | _ -> count)
      0
      members
  in
  (* Check for non-string, non-defaulted members *)
  List.iter
    (fun member ->
      match member with
      | StringMember _
      | DefaultedMember _ ->
        ()
      | _ ->
        let loc = member_loc member in
        let name = member_name member in
        errors :=
          InvalidMemberInitializer { loc; explicit_type = Some String; member_name = name }
          :: !errors)
    members;
  (* Check for mix of initialized and defaulted string members *)
  if string_count > 0 && defaulted_count > 0 then
    if defaulted_count > string_count then
      List.iter
        (fun member ->
          match member with
          | StringMember (loc, _) ->
            errors := StringMemberInconsistentlyInitialized { loc } :: !errors
          | _ -> ())
        members
    else
      List.iter
        (fun member ->
          match member with
          | DefaultedMember (loc, _) ->
            errors := StringMemberInconsistentlyInitialized { loc } :: !errors
          | _ -> ())
        members;
  List.rev !errors

let validate_explicit_symbol members =
  let errors = ref [] in
  List.iter
    (fun member ->
      match member with
      | DefaultedMember _ -> ()
      | _ ->
        let loc = member_loc member in
        let name = member_name member in
        errors := SymbolMemberWithInitializer { loc; member_name = name } :: !errors)
    members;
  List.rev !errors

let validate_explicit_bigint members =
  let errors = ref [] in
  List.iter
    (fun member ->
      match member with
      | BigIntMember _ -> ()
      | DefaultedMember (loc, { DefaultedMember.id }) ->
        errors :=
          BigIntMemberNotInitialized
            { loc; member_name = Flow_ast_utils.string_of_enum_member_name id }
          :: !errors
      | _ ->
        let loc = member_loc member in
        let name = member_name member in
        errors :=
          InvalidMemberInitializer { loc; explicit_type = Some BigInt; member_name = name }
          :: !errors)
    members;
  List.rev !errors

(* For implicit enums, infer the type from the members *)
let validate_implicit_type ~body_loc members =
  let counts = count_members members in
  let { boolean_count; number_count; string_count; bigint_count; defaulted_count } = counts in
  match (boolean_count, number_count, bigint_count, string_count, defaulted_count) with
  | (0, 0, 0, 0, 0) ->
    (* Empty enum - defaults to string *)
    (Some (StringRep { truthy = true }), [])
  | (0, 0, 0, _, _) ->
    (* Only strings and/or defaulted - this is a string enum *)
    let errors = ref [] in
    if string_count > 0 && defaulted_count > 0 then
      if defaulted_count > string_count then (
        List.iter
          (fun member ->
            match member with
            | StringMember (loc, _) ->
              errors := StringMemberInconsistentlyInitialized { loc } :: !errors
            | _ -> ())
          members;
        (Some (StringRep { truthy = true }), List.rev !errors)
      ) else (
        List.iter
          (fun member ->
            match member with
            | DefaultedMember (loc, _) ->
              errors := StringMemberInconsistentlyInitialized { loc } :: !errors
            | _ -> ())
          members;
        let truthy =
          List.for_all
            (fun member ->
              match member with
              | StringMember
                  (_, { InitializedMember.init = (_, { Flow_ast.StringLiteral.value; _ }); _ }) ->
                value <> ""
              | _ -> true)
            members
        in
        (Some (StringRep { truthy }), List.rev !errors)
      )
    else if string_count > 0 then
      let truthy =
        List.for_all
          (fun member ->
            match member with
            | StringMember
                (_, { InitializedMember.init = (_, { Flow_ast.StringLiteral.value; _ }); _ }) ->
              value <> ""
            | _ -> true)
          members
      in
      (Some (StringRep { truthy }), [])
    else
      (Some (StringRep { truthy = true }), [])
  | (_, 0, 0, 0, _) when boolean_count >= defaulted_count ->
    (* Majority booleans - boolean enum *)
    let errors = ref [] in
    List.iter
      (fun member ->
        match member with
        | DefaultedMember (loc, { DefaultedMember.id }) ->
          errors :=
            BooleanMemberNotInitialized
              { loc; member_name = Flow_ast_utils.string_of_enum_member_name id }
            :: !errors
        | _ -> ())
      members;
    let lit =
      List.fold_left
        (fun acc member ->
          match member with
          | BooleanMember
              (_, { InitializedMember.init = (_, { Flow_ast.BooleanLiteral.value; _ }); _ }) ->
            (match acc with
            | None -> Some value
            | Some _ -> None)
          | _ -> acc)
        None
        members
    in
    (Some (BoolRep lit), List.rev !errors)
  | (0, _, 0, 0, _) when number_count >= defaulted_count ->
    (* Majority numbers - number enum *)
    let errors = ref [] in
    List.iter
      (fun member ->
        match member with
        | DefaultedMember (loc, { DefaultedMember.id }) ->
          errors :=
            NumberMemberNotInitialized
              { loc; member_name = Flow_ast_utils.string_of_enum_member_name id }
            :: !errors
        | _ -> ())
      members;
    let truthy =
      List.for_all
        (fun member ->
          match member with
          | NumberMember
              (_, { InitializedMember.init = (_, { Flow_ast.NumberLiteral.value; _ }); _ }) ->
            value <> 0.0
          | _ -> true)
        members
    in
    (Some (NumberRep { truthy }), List.rev !errors)
  | (0, 0, _, 0, _) when bigint_count >= defaulted_count ->
    (* Majority bigints - bigint enum *)
    let errors = ref [] in
    List.iter
      (fun member ->
        match member with
        | DefaultedMember (loc, { DefaultedMember.id }) ->
          errors :=
            BigIntMemberNotInitialized
              { loc; member_name = Flow_ast_utils.string_of_enum_member_name id }
            :: !errors
        | _ -> ())
      members;
    let truthy =
      List.for_all
        (fun member ->
          match member with
          | BigIntMember
              (_, { InitializedMember.init = (_, { Flow_ast.BigIntLiteral.value; _ }); _ }) ->
            value <> Some 0L
          | _ -> true)
        members
    in
    (Some (BigIntRep { truthy }), List.rev !errors)
  | _ ->
    (* Mixed types - error *)
    (None, [InconsistentMemberValues { loc = body_loc }])

let compute_rep_for_explicit_type explicit_type members =
  match explicit_type with
  | Boolean ->
    let lit =
      List.fold_left
        (fun acc member ->
          match member with
          | BooleanMember
              (_, { InitializedMember.init = (_, { Flow_ast.BooleanLiteral.value; _ }); _ }) ->
            (match acc with
            | None -> Some value
            | Some _ -> None)
          | _ -> acc)
        None
        members
    in
    BoolRep lit
  | Number ->
    let truthy =
      List.for_all
        (fun member ->
          match member with
          | NumberMember
              (_, { InitializedMember.init = (_, { Flow_ast.NumberLiteral.value; _ }); _ }) ->
            value <> 0.0
          | _ -> true)
        members
    in
    NumberRep { truthy }
  | String ->
    let has_initialized =
      List.exists
        (fun member ->
          match member with
          | StringMember _ -> true
          | _ -> false)
        members
    in
    if has_initialized then
      let truthy =
        List.for_all
          (fun member ->
            match member with
            | StringMember
                (_, { InitializedMember.init = (_, { Flow_ast.StringLiteral.value; _ }); _ }) ->
              value <> ""
            | _ -> true)
          members
      in
      StringRep { truthy }
    else
      StringRep { truthy = true }
  | Symbol -> SymbolRep
  | BigInt ->
    let truthy =
      List.for_all
        (fun member ->
          match member with
          | BigIntMember
              (_, { InitializedMember.init = (_, { Flow_ast.BigIntLiteral.value; _ }); _ }) ->
            value <> Some 0L
          | _ -> true)
        members
    in
    BigIntRep { truthy }

let extract_member_names members =
  List.fold_left
    (fun acc member ->
      let name = member_name member in
      let loc = member_loc member in
      if name = "" then
        acc
      else
        (name, loc) :: acc)
    []
    members
  |> List.rev

type 'M classification_result = {
  rep: 'M enum_rep option;
  members: (string * 'M) list;
  has_unknown_members: 'M option;
  errors: 'M validation_error list;
}

let classify_enum_body (body : 'M Body.t) ~body_loc : 'M classification_result =
  let { Body.members; explicit_type; has_unknown_members; comments = _ } = body in
  let dup_errors = check_duplicate_names members in
  let member_names = extract_member_names members in
  let (rep, type_errors) =
    match explicit_type with
    | Some (_, Boolean) ->
      let errs = validate_explicit_boolean members in
      (Some (compute_rep_for_explicit_type Boolean members), errs)
    | Some (_, Number) ->
      let errs = validate_explicit_number members in
      (Some (compute_rep_for_explicit_type Number members), errs)
    | Some (_, String) ->
      let errs = validate_explicit_string members in
      (Some (compute_rep_for_explicit_type String members), errs)
    | Some (_, Symbol) ->
      let errs = validate_explicit_symbol members in
      (Some SymbolRep, errs)
    | Some (_, BigInt) ->
      let errs = validate_explicit_bigint members in
      (Some (compute_rep_for_explicit_type BigInt members), errs)
    | None -> validate_implicit_type ~body_loc members
  in
  let name_errors = check_member_names members in
  let non_id_errors = check_non_identifier_member_names members in
  let value_errors = check_duplicate_values members in
  {
    rep;
    members = member_names;
    has_unknown_members;
    errors = dup_errors @ type_errors @ name_errors @ non_id_errors @ value_errors;
  }
