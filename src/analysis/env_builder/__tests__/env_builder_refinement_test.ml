(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test_utils
module ALocMap = Loc_collections.ALocMap
module ALocSet = Loc_collections.ALocSet

let jsx_mode = ref Options.Jsx_react

let react_runtime = ref Options.ReactRuntimeClassic

module TestCx = struct
  type t = unit

  let enable_enums _ = true

  let file _cx = File_key.SourceFile "test.js"

  let jsx _cx = !jsx_mode

  let react_runtime _cx = !react_runtime

  let enable_const_params _cx = false

  let add_literal_subtypes _ _ = ()

  let add_exhaustive_check _ _ _ = ()

  let exhaustive_check _ _ = ([], false)
end

module Name_resolver = Name_resolver.Make_Test_With_Cx (TestCx)

let print_values refinement_of_id =
  let open Name_resolver.Env_api in
  let rec print_value write_loc =
    match write_loc with
    | Uninitialized _ -> "(uninitialized)"
    | Undeclared _ -> "(undeclared)"
    | Projection l -> Utils_js.spf "projection at %s" (L.debug_to_string l)
    | Write reason ->
      let loc = Reason.loc_of_reason reason in
      Utils_js.spf
        "%s: (%s)"
        (L.debug_to_string loc)
        Reason.(desc_of_reason reason |> string_of_desc)
    | EmptyArray { reason; _ } ->
      let loc = Reason.loc_of_reason reason in
      Utils_js.spf
        "(empty array) %s: (%s)"
        (L.debug_to_string loc)
        Reason.(desc_of_reason reason |> string_of_desc)
    | IllegalWrite reason ->
      let loc = Reason.loc_of_reason reason in
      Utils_js.spf "illegal write at %s" (L.debug_to_string loc)
    | Refinement { refinement_id; writes; write_id = _ } ->
      let { Env_api.Refi.kind; _ } = refinement_of_id refinement_id in
      let refinement_str = show_refinement_kind_without_locs kind in
      let writes_str = String.concat "," (List.map print_value writes) in
      Printf.sprintf "{refinement = %s; writes = %s}" refinement_str writes_str
    | FunctionThis _ -> "This(function)"
    | GlobalThis _ -> "This(global)"
    | IllegalThis _ -> "This(illegal)"
    | ClassInstanceThis _ -> "This(instance)"
    | ClassStaticThis _ -> "This(static)"
    | ClassInstanceSuper _ -> "Super(instance)"
    | ClassStaticSuper _ -> "Super(static)"
    | ModuleScoped name -> "ModuleScoped " ^ name
    | Global name -> "Global " ^ name
    | Unreachable _ -> "unreachable"
    | Undefined _ -> "undefined"
    | Number _ -> "number"
    | DeclaredFunction l -> Printf.sprintf "declared function %s" (L.debug_to_string l)
  in
  let print_invalidation map =
    map
    |> L.LMap.bindings
    |> Base.List.map ~f:(fun (loc, reason) ->
           let reason = Refinement_invalidation.string_of_reason reason in
           Printf.sprintf "%s at %s" reason (L.debug_to_string loc)
       )
    |> Base.String.concat ~sep:",\n    "
  in
  fun values invalidations ->
    let strlist =
      Base.List.map
        ~f:(fun (read_loc, { def_loc = _; val_kind = _; write_locs; name = _; id = _ }) ->
          Printf.sprintf
            "%s => {\n    %s\n  }"
            (L.debug_to_string read_loc)
            (String.concat ",\n    " @@ Base.List.map ~f:print_value write_locs))
        (L.LMap.bindings values)
      @ Base.List.map
          ~f:(fun (read_loc, invalidation) ->
            Printf.sprintf
              "%s => invalidated refinement by {\n    %s\n  }"
              (L.debug_to_string read_loc)
              (print_invalidation invalidation))
          (L.LMap.bindings invalidations)
    in
    Printf.printf "[\n  %s]" (String.concat ";\n  " strlist)

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let print_ssa_test ?(custom_jsx = None) ?(react_runtime_automatic=false) ?lib ?exclude_syms contents =
  if react_runtime_automatic then (
    react_runtime := Options.ReactRuntimeAutomatic
  );
  (match custom_jsx with
  | None -> ()
  | Some str ->
      let (ast, _errors) = Parser_flow.jsx_pragma_expression str None in
      let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#expression ast in
      jsx_mode := Options.Jsx_pragma (str, aloc_ast)

  );
  let aloc_ast = parse_with_alocs contents in
  let refined_reads, invalidations, refinement_of_id = Name_resolver.program () ?lib ?exclude_syms aloc_ast in
  print_values refinement_of_id refined_reads invalidations;
  react_runtime := Options.ReactRuntimeClassic;
  jsx_mode := Options.Jsx_react

let%expect_test "conj_refinement_empty" =
  print_ssa_test {|
function foo(e: Error) {
  if (e instanceof Error && true) {}
  else { e }
}
|};
    [%expect {|
      [
        (2, 16) to (2, 21) => {
          Global Error
        };
        (3, 6) to (3, 7) => {
          (2, 13) to (2, 14): (`e`)
        };
        (3, 19) to (3, 24) => {
          Global Error
        };
        (4, 9) to (4, 10) => {
          (2, 13) to (2, 14): (`e`)
        }] |}]

let%expect_test "logical_expr" =
  print_ssa_test {|let x = null;
let y = null;
(x && (y = x)) + x|};
  [%expect {|
    [
      (3, 1) to (3, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (3, 11) to (3, 12) => {
        {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
      };
      (3, 17) to (3, 18) => {
        (1, 4) to (1, 5): (`x`)
      }] |}]

let%expect_test "logical_expr_successive" =
  print_ssa_test {|let x = null;
x && (x && x)|};
  [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
      };
      (2, 11) to (2, 12) => {
        {refinement = Truthy; writes = {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}}
      }] |}]

let%expect_test "logical_or" =
  print_ssa_test {|let x = null;
x || x|};
  [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 5) to (2, 6) => {
        {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "logical_nc_and" =
  print_ssa_test {|let x = null;
(x ?? x) && x|};
  [%expect{|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
      };
      (2, 12) to (2, 13) => {
        {refinement = Or (And (Not (Maybe), Truthy), Truthy); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "logical_nc_no_key" =
  print_ssa_test {|let x = null;
((x != null) ?? x) && x|};
  [%expect {|
    [
      (2, 2) to (2, 3) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 16) to (2, 17) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 22) to (2, 23) => {
        (1, 4) to (1, 5): (`x`)
      }] |}]

let%expect_test "logical_nested_right" =
  print_ssa_test {|let x = null;
x || (x || x)|};
  [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
      };
      (2, 11) to (2, 12) => {
        {refinement = Not (Truthy); writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}}
      }] |}]

let%expect_test "logical_nested" =
  print_ssa_test {|let x = null;
(x || (x != null)) && x|};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 7) to (2, 8) => {
        {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
      };
      (2, 22) to (2, 23) => {
        {refinement = Or (Truthy, Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "logical_nested2" =
  print_ssa_test {|let x = null;
(x && x) || (x && x)|};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
      };
      (2, 13) to (2, 14) => {
        {refinement = Or (Not (Truthy), Not (Truthy)); writes = (1, 4) to (1, 5): (`x`)}
      };
      (2, 18) to (2, 19) => {
        {refinement = Truthy; writes = {refinement = Or (Not (Truthy), Not (Truthy)); writes = (1, 4) to (1, 5): (`x`)}}
      }] |}]

let%expect_test "logical_assignment_and" =
  print_ssa_test {|let x = null;
x &&= x;|}; [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "logical_assignment_or" =
  print_ssa_test {|let x = null;
x ||= x;|}; [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "logical_assignment_nullish" =
  print_ssa_test {|let x = null;
x ??= x;|}; [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 7) => {
        {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "logical_assignment_and_throws" =
  print_ssa_test {|let x = null;
x &&= invariant(false);|}; [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 15) => {
        Global invariant
      }] |}]

let%expect_test "logical_assignment_or_throws" =
  print_ssa_test {|let x = null;
x ||= invariant(false);|}; [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 15) => {
        Global invariant
      }] |}]

let%expect_test "logical_assignment_nullish_throws" =
  print_ssa_test {|let x = null;
x ??= invariant(false);|}; [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 15) => {
        Global invariant
      }] |}]

let%expect_test "assignment_truthy" =
  print_ssa_test {|let x = null;
(x = null) && x|};
  [%expect {|
    [
      (2, 14) to (2, 15) => {
        {refinement = Truthy; writes = (2, 1) to (2, 2): (`x`)}
      }] |}]

let%expect_test "eq_null" =
  print_ssa_test {|let x = null;
(x == null) && x|};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 15) to (2, 16) => {
        {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "neq_null" =
  print_ssa_test {|let x = null;
(x != null) && x|};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 15) to (2, 16) => {
        {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_eq_null" =
  print_ssa_test {|let x = null;
(x === null) && x|};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 16) to (2, 17) => {
        {refinement = Null; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_neq_null" =
  print_ssa_test {|let x = null;
(x !== null) && x|};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 16) to (2, 17) => {
        {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_eq_null_sentinel" =
  print_ssa_test {|
if (o.err === null) {
  o
} else {
  o;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global o
        };
        (3, 2) to (3, 3) => {
          {refinement = SentinelR err; writes = Global o}
        };
        (5, 2) to (5, 3) => {
          {refinement = Not (SentinelR err); writes = Global o}
        }]
      |}]

let%expect_test "strict_neq_null_sentinel" =
  print_ssa_test {|
if (o.err !== null) {
  o
} else {
  o;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global o
        };
        (3, 2) to (3, 3) => {
          {refinement = Not (SentinelR err); writes = Global o}
        };
        (5, 2) to (5, 3) => {
          {refinement = SentinelR err; writes = Global o}
        }]
      |}]

let%expect_test "eq_undefined" =
  print_ssa_test {|let x = undefined;
(x == undefined) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 15) => {
        Global undefined
      };
      (2, 20) to (2, 21) => {
        {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "neq_undefined" =
  print_ssa_test {|let x = undefined;
(x != undefined) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 6) to (2, 15) => {
        Global undefined
      };
      (2, 20) to (2, 21) => {
        {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_eq_undefined" =
  print_ssa_test {|let x = undefined;
(x === undefined) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 7) to (2, 16) => {
        Global undefined
      };
      (2, 21) to (2, 22) => {
        {refinement = Not (Not (Undefined)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_neq_undefined" =
  print_ssa_test {|let x = undefined;
(x !== undefined) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 7) to (2, 16) => {
        Global undefined
      };
      (2, 21) to (2, 22) => {
        {refinement = Not (Undefined); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_eq_undefined_sentinel" =
  print_ssa_test {|
if (o.err === undefined) {
  o
} else {
  o;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global o
        };
        (2, 14) to (2, 23) => {
          Global undefined
        };
        (3, 2) to (3, 3) => {
          {refinement = Not (Not (SentinelR err)); writes = Global o}
        };
        (5, 2) to (5, 3) => {
          {refinement = Not (SentinelR err); writes = Global o}
        }]
      |}]

let%expect_test "strict_neq_undefined_sentinel" =
  print_ssa_test {|
if (o.err !== undefined) {
  o
} else {
  o;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global o
        };
        (2, 14) to (2, 23) => {
          Global undefined
        };
        (3, 2) to (3, 3) => {
          {refinement = Not (SentinelR err); writes = Global o}
        };
        (5, 2) to (5, 3) => {
          {refinement = Not (Not (SentinelR err)); writes = Global o}
        }]
      |}]

let%expect_test "undefined_already_bound" =
  print_ssa_test {|let undefined = 3;
let x = null;
(x !== undefined) && x|};
  [%expect {|
    [
      (3, 1) to (3, 2) => {
        (2, 4) to (2, 5): (`x`)
      };
      (3, 7) to (3, 16) => {
        (1, 4) to (1, 13): (`undefined`)
      };
      (3, 21) to (3, 22) => {
        (2, 4) to (2, 5): (`x`)
      }] |}]

let%expect_test "eq_void" =
  print_ssa_test {|let x = undefined;
(x == void 0) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 17) to (2, 18) => {
        {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "neq_void" =
  print_ssa_test {|let x = undefined;
(x != void 0) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 17) to (2, 18) => {
        {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_eq_void" =
  print_ssa_test {|let x = undefined;
(x === void 0) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 18) to (2, 19) => {
        {refinement = Not (Not (Undefined)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "strict_neq_void" =
  print_ssa_test {|let x = undefined;
(x !== void 0) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 18) to (2, 19) => {
        {refinement = Not (Undefined); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "instanceof" =
  print_ssa_test {|let x = undefined;
(x instanceof Object) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 14) to (2, 20) => {
        Global Object
      };
      (2, 25) to (2, 26) => {
        {refinement = instanceof; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "Array.isArray" =
  print_ssa_test {|let x = undefined;
(Array.isArray(x)) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 6) => {
        Global Array
      };
      (2, 15) to (2, 16) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 22) to (2, 23) => {
        {refinement = isArray; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "unary_negation" =
  print_ssa_test {|let x = undefined;
(!Array.isArray(x)) && x;
!x && x;
!(x || x) && x;|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 2) to (2, 7) => {
        Global Array
      };
      (2, 16) to (2, 17) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 23) to (2, 24) => {
        {refinement = Not (isArray); writes = (1, 4) to (1, 5): (`x`)}
      };
      (3, 1) to (3, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (3, 6) to (3, 7) => {
        {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
      };
      (4, 2) to (4, 3) => {
        (1, 4) to (1, 5): (`x`)
      };
      (4, 7) to (4, 8) => {
        {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
      };
      (4, 13) to (4, 14) => {
        {refinement = And (Not (Truthy), Not (Truthy)); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_bool" =
  print_ssa_test {|let x = undefined;
(typeof x == "boolean") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 27) to (2, 28) => {
        {refinement = bool; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_bool" =
  print_ssa_test {|let x = undefined;
(typeof x != "boolean") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 27) to (2, 28) => {
        {refinement = Not (bool); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_number" =
  print_ssa_test {|let x = undefined;
(typeof x == "number") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = number; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_number" =
  print_ssa_test {|let x = undefined;
(typeof x != "number") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (number); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_function" =
  print_ssa_test {|let x = undefined;
(typeof x == "function") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 28) to (2, 29) => {
        {refinement = function; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_function" =
  print_ssa_test {|let x = undefined;
(typeof x != "function") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 28) to (2, 29) => {
        {refinement = Not (function); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_object" =
  print_ssa_test {|let x = undefined;
(typeof x == "object") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = object; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_object" =
  print_ssa_test {|let x = undefined;
(typeof x != "object") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (object); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_string" =
  print_ssa_test {|let x = undefined;
(typeof x == "string") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = string; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_string" =
  print_ssa_test {|let x = undefined;
(typeof x != "string") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (string); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_symbol" =
  print_ssa_test {|let x = undefined;
(typeof x == "symbol") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = symbol; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_symbol" =
  print_ssa_test {|let x = undefined;
(typeof x != "symbol") && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (symbol); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_bool_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `boolean`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 27) to (2, 28) => {
        {refinement = bool; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_bool_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `boolean`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 27) to (2, 28) => {
        {refinement = Not (bool); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_number_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `number`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = number; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_number_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `number`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (number); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_function_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `function`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 28) to (2, 29) => {
        {refinement = function; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_function_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `function`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 28) to (2, 29) => {
        {refinement = Not (function); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_object_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `object`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = object; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_object_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `object`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (object); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_string_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `string`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = string; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_string_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `string`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (string); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "typeof_symbol_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `symbol`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = symbol; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "not_typeof_symbol_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `symbol`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 8) to (2, 9) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 26) to (2, 27) => {
        {refinement = Not (symbol); writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "singleton_bool" =
  print_ssa_test {|let x = undefined;
(x === true) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 16) to (2, 17) => {
        {refinement = true; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "singleton_str" =
  print_ssa_test {|let x = undefined;
(x === "str") && x|};
  [%expect{|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 17) to (2, 18) => {
        {refinement = str; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "singleton_str_template" =
  print_ssa_test {|let x = undefined;
(x === `str`) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 17) to (2, 18) => {
        {refinement = str; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "singleton_num" =
  print_ssa_test {|let x = undefined;
(x === 3) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 13) to (2, 14) => {
        {refinement = 3; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "singleton_num_neg" =
  print_ssa_test {|let x = undefined;
(x === -3) && x|};
  [%expect {|
    [
      (1, 8) to (1, 17) => {
        Global undefined
      };
      (2, 1) to (2, 2) => {
        (1, 4) to (1, 5): (`x`)
      };
      (2, 14) to (2, 15) => {
        {refinement = -3; writes = (1, 4) to (1, 5): (`x`)}
      }] |}]

let%expect_test "sentinel_lit" =
  print_ssa_test {|let x = undefined;
(x.foo === 3) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 17) to (2, 18) => {
          {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "sentinel_lit_indexed " =
  print_ssa_test {|let x = undefined;
(x["foo"] === 3) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 20) to (2, 21) => {
          {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "sentinel_nonlit" =
  print_ssa_test {|let x = undefined;
let y = undefined;
(x.foo === y) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 1) to (3, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 11) to (3, 12) => {
          (2, 4) to (2, 5): (`y`)
        };
        (3, 17) to (3, 18) => {
          {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "sentinel_nonlit_indexed" =
  print_ssa_test {|let x = undefined;
let y = undefined;
(x["foo"] === y) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 1) to (3, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 14) to (3, 15) => {
          (2, 4) to (2, 5): (`y`)
        };
        (3, 20) to (3, 21) => {
          {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "refined_call_in_member_expressions" =
  print_ssa_test {|let x = undefined;
if (x.foo != null && x.foo.bar()) {}|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 21) to (2, 22) => {
          {refinement = Not (PropNullishR foo); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 21) to (2, 26) => {
          {refinement = Not (Maybe); writes = projection at (2, 4) to (2, 9)}
        }]
       |}]

let%expect_test "refined_call_in_unrefinable_member_expressions" =
  print_ssa_test {|let x = undefined;
if (x.foo != null && x.foo.bar()[0] === BAZ) {}|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 21) to (2, 22) => {
          {refinement = Not (PropNullishR foo); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 21) to (2, 26) => {
          {refinement = Not (Maybe); writes = projection at (2, 4) to (2, 9)}
        };
        (2, 40) to (2, 43) => {
          Global BAZ
        }]
       |}]

let%expect_test "optional_chain_lit" =
  print_ssa_test {|let x = undefined;
(x?.foo === 3) ? x : x|};
    [%expect{|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 17) to (2, 18) => {
          {refinement = And (And (Not (Maybe), Not (PropNullishR foo)), SentinelR foo); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 21) to (2, 22) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (Not (PropNullishR foo))), Not (SentinelR foo)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "optional_chain_not_lit" =
  print_ssa_test {|let x = undefined;
(x?.foo !== 3) ? x : x|};
    [%expect{|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 17) to (2, 18) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (Not (PropNullishR foo))), Not (SentinelR foo)); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 21) to (2, 22) => {
          {refinement = And (And (Not (Maybe), Not (PropNullishR foo)), SentinelR foo); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "optional_chain_bad_prop_truthy_r" =
  print_ssa_test {|
  function withResult1(result: {ok: true} | {ok: false}): string {
    if(result?.ok === false) {
        return result as empty; // bad: result is not empty
    }
    return "Hello"
  }
  function withResult2(result: {ok: true} | {ok: void}): string {
    if(result?.ok === undefined) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
  function withResult3(result: {ok: true} | {ok: null}): string {
    if(result?.ok === null) {
        return result as empty; // bad: result is not empty
    }
    return "Hello"
  }
  function withResult4(result: {ok: true} | {ok: null}): string {
    if(result?.ok == null) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
  function withResult5(result: {ok: true} | {ok: null}): string {
    if(result?.ok == undefined) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
|};
    [%expect{|
      [
        (3, 7) to (3, 13) => {
          (2, 23) to (2, 29): (`result`)
        };
        (4, 15) to (4, 21) => {
          {refinement = And (And (Not (Maybe), Not (PropNullishR ok)), SentinelR ok); writes = (2, 23) to (2, 29): (`result`)}
        };
        (9, 7) to (9, 13) => {
          (8, 23) to (8, 29): (`result`)
        };
        (9, 22) to (9, 31) => {
          Global undefined
        };
        (10, 15) to (10, 21) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (PropNonVoidR ok)), Not (Not (SentinelR ok))); writes = (8, 23) to (8, 29): (`result`)}
        };
        (15, 7) to (15, 13) => {
          (14, 23) to (14, 29): (`result`)
        };
        (16, 15) to (16, 21) => {
          {refinement = And (And (Not (Maybe), PropIsExactlyNullR ok), SentinelR ok); writes = (14, 23) to (14, 29): (`result`)}
        };
        (21, 7) to (21, 13) => {
          (20, 23) to (20, 29): (`result`)
        };
        (22, 15) to (22, 21) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (Not (PropNullishR ok))), Not (Not (PropNullishR ok))); writes = (20, 23) to (20, 29): (`result`)}
        };
        (27, 7) to (27, 13) => {
          (26, 23) to (26, 29): (`result`)
        };
        (27, 21) to (27, 30) => {
          Global undefined
        };
        (28, 15) to (28, 21) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (Not (PropNullishR ok))), Not (Not (PropNullishR ok))); writes = (26, 23) to (26, 29): (`result`)}
        }] |}]

let%expect_test "optional_chain_member_base" =
  print_ssa_test {|let x = undefined;
(x.foo?.bar === 3) ? x : x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 21) to (2, 22) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 25) to (2, 26) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "optional_chain_with_call" =
  print_ssa_test {|let x = undefined;
(x?.foo().bar === 3) ? x : x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 23) to (2, 24) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 27) to (2, 28) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "optional_multiple_chains" =
  print_ssa_test {|let x = undefined;
(x?.foo?.bar.baz?.qux === 3) ? x : x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 31) to (2, 32) => {
          {refinement = And (Not (Maybe), Not (PropNullishR foo)); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 35) to (2, 36) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "optional_base_call" =
  print_ssa_test {|let x = undefined;
(x?.().foo?.bar.baz?.qux === 3) ? x : x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 34) to (2, 35) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 38) to (2, 39) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "sentinel_standalone" =
  print_ssa_test {|let x = undefined;
x.foo && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 1) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 9) to (2, 10) => {
          {refinement = PropTruthyR (foo); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "optional_chain_standalone" =
  print_ssa_test {|let x = undefined;
x?.foo && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 1) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 10) to (2, 11) => {
          {refinement = And (Not (Maybe), PropTruthyR (foo)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "no_sentinel_in_non_strict" =
  print_ssa_test {|
var x : {p:?string} = {p:"xxx"};
if (x.p != null) {
  alert("");
  x.p;
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 2) to (4, 7) => {
          Global alert
        };
        (5, 2) to (5, 3) => {
          {refinement = Not (PropNullishR p); writes = (2, 4) to (2, 5): (`x`)}
        };
        (5, 2) to (5, 5) => invalidated refinement by {
          function call at (4, 2) to (4, 11)
        }] |}]

let%expect_test "conditional_expression" =
  print_ssa_test {|let x = undefined;
(x ? x: x) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 5) to (2, 6) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 8) to (2, 9) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 14) to (2, 15) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "conditional_throw" =
  print_ssa_test {|let x = undefined;
(x ? invariant() : x) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 5) to (2, 14) => {
          Global invariant
        };
        (2, 19) to (2, 20) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 25) to (2, 26) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "conditional_throw2" =
  print_ssa_test {|let x = undefined;
(x ? x : invariant()) && x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 5) to (2, 6) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (2, 9) to (2, 18) => {
          Global invariant
        };
        (2, 25) to (2, 26) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "logical_throw_and" =
  print_ssa_test {|let x = undefined;
x && invariant();
x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 1) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 5) to (2, 14) => {
          Global invariant
        };
        (3, 0) to (3, 1) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "logical_throw_or" =
  print_ssa_test {|let x = undefined;
x || invariant();
x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 1) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 5) to (2, 14) => {
          Global invariant
        };
        (3, 0) to (3, 1) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "logical_throw_nc" =
  print_ssa_test {|let x = undefined;
x ?? invariant();
x|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 1) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 5) to (2, 14) => {
          Global invariant
        };
        (3, 0) to (3, 1) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "logical_throw_reassignment" =
  print_ssa_test {|let x = undefined;
try {
  x ?? invariant(false, x = 3);
} finally {
  x;
}|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 2) to (3, 3) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 7) to (3, 16) => {
          Global invariant
        };
        (5, 2) to (5, 3) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "nested_logical_throw_and" =
  print_ssa_test {|let x = undefined;
(x && invariant()) && x;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 6) to (2, 15) => {
          Global invariant
        };
        (2, 22) to (2, 23) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (3, 0) to (3, 1) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "nested_logical_throw_or" =
  print_ssa_test {|let x = undefined;
(x || invariant()) && x;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 6) to (2, 15) => {
          Global invariant
        };
        (2, 22) to (2, 23) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (3, 0) to (3, 1) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "nested_logical_throw_nc" =
  print_ssa_test {|let x = undefined;
(x ?? invariant()) && x;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 1) to (2, 2) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 6) to (2, 15) => {
          Global invariant
        };
        (2, 22) to (2, 23) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        };
        (3, 0) to (3, 1) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "if_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  x;
} else {
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 3) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (5, 2) to (5, 3) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        };
        (7, 0) to (7, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "if_no_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 3) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "if_no_else_statement_with_assignment_simple" =
  print_ssa_test {|let x = 0;
if (true) {
  x = null;
}
x;|};
    [%expect {|
      [
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "if_no_else_statement_with_assignment_simple_with_annotation" =
  print_ssa_test {|let x: number = 0;
if (true) {
  x = null;
}
x;|};
    [%expect {|
      [
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "if_no_else_statement_with_assignment_uninitialized_with_annotation" =
  print_ssa_test {|let x: number;
if (true) {
  x = 1;
}
x;|};
    [%expect {|
      [
        (5, 0) to (5, 1) => {
          (uninitialized),
          (3, 2) to (3, 3): (`x`)
        }] |}]

let%expect_test "if_no_else_statement_with_assignment_with_refinements" =
  print_ssa_test {|let x = undefined;
if (x !== null) {
  x = null;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          (3, 2) to (3, 3): (`x`),
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "if_throw_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  throw 'error';
} else {
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 2) to (5, 3) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        };
        (7, 0) to (7, 1) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "if_else_throw_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  x;
} else {
  throw 'error';
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 3) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (7, 0) to (7, 1) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "if_return_else_statement" =
  print_ssa_test {|function f() {
  let x = undefined;
  if (x) {
    return;
  } else {
    x;
  }
  x;
}
|};
    [%expect{|
      [
        (2, 10) to (2, 19) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (2, 6) to (2, 7): (`x`)
        };
        (6, 4) to (6, 5) => {
          {refinement = Not (Truthy); writes = (2, 6) to (2, 7): (`x`)}
        };
        (8, 2) to (8, 3) => {
          {refinement = Not (Truthy); writes = (2, 6) to (2, 7): (`x`)}
        }] |}]

let%expect_test "if_else_return_statement" =
  print_ssa_test {|function f() {
  let x = undefined;
  if (x) {
    x;
  } else {
    return;
  }
  x;
}
|};
    [%expect{|
      [
        (2, 10) to (2, 19) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (2, 6) to (2, 7): (`x`)
        };
        (4, 4) to (4, 5) => {
          {refinement = Truthy; writes = (2, 6) to (2, 7): (`x`)}
        };
        (8, 2) to (8, 3) => {
          {refinement = Truthy; writes = (2, 6) to (2, 7): (`x`)}
        }] |}]

let%expect_test "nested_if_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  if (x === null) {
    throw 'error';
  }
  x;
} else {
  if (x === null) {
    x;
  } else {
    throw 'error';
  }
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 6) to (3, 7) => {
          {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Null); writes = {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}}
        };
        (8, 6) to (8, 7) => {
          {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}
        };
        (9, 4) to (9, 5) => {
          {refinement = Null; writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (13, 2) to (13, 3) => {
          {refinement = Null; writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (15, 0) to (15, 1) => {
          {refinement = Not (Null); writes = {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}},
          {refinement = Null; writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}}
        }] |}]

let%expect_test "while" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 7) to (2, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "while_assign" =
  print_ssa_test {|
declare var x: string;
while (x = x) { x }
|};
    [%expect {|
      [
        (3, 11) to (3, 12) => {
          (2, 12) to (2, 13): (`x`)
        };
        (3, 16) to (3, 17) => {
          {refinement = Truthy; writes = (3, 7) to (3, 8): (`x`)}
        }]
       |}]

let%expect_test "while_throw" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  throw 'error';
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 7) to (2, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "while_break_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
while (x != null) {
  if (y == null) {
    break;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 7) to (3, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)}
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "while_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
while (x != null) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 7) to (3, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          (2, 4) to (2, 5): (`y`)
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "while_with_var_write" =
  print_ssa_test {|
while (true) {
  var a = function() {}
}
a()|};
  [%expect {|
    [
      (5, 0) to (5, 1) => {
        (uninitialized),
        (3, 6) to (3, 7): (`a`)
      }]
      |}]

let%expect_test "while_continue" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  continue;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 7) to (2, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "while_continue_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
while (x != null) {
  if (y == null) {
    continue;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 7) to (3, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)}
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "while_phi_node_refinement" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  if (x === 3) {
    continue;
  }
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 7) to (2, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 6) to (3, 7) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (3); writes = {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (8, 0) to (8, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "do_while" =
  print_ssa_test {|let x = undefined;
do {
  x;
} while (x != null);
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 2) to (3, 3) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 9) to (4, 10) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "do_while_break_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
do {
  if (y == null) {
    break;
  }
  y;
} while (x != null);
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)}
        };
        (8, 9) to (8, 10) => {
          (1, 4) to (1, 5): (`x`)
        };
        (9, 0) to (9, 1) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)},
          {refinement = Not (Not (Maybe)); writes = (2, 4) to (2, 5): (`y`)}
        };
        (10, 0) to (10, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "do_while_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
do {
  if (y == null) {
    x = 2;
  }
  y;
} while (x != null);
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          (2, 4) to (2, 5): (`y`)
        };
        (8, 9) to (8, 10) => {
          (1, 4) to (1, 5): (`x`)
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "do_while_continue" =
  print_ssa_test {|let x = undefined;
do {
  continue;
} while (x != null);
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (4, 9) to (4, 10) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "do_while_continue_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
do {
  if (y == null) {
    continue;
  }
  y;
} while (x != null);
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)}
        };
        (8, 9) to (8, 10) => {
          (1, 4) to (1, 5): (`x`)
        };
        (9, 0) to (9, 1) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)},
          {refinement = Not (Not (Maybe)); writes = (2, 4) to (2, 5): (`y`)}
        };
        (10, 0) to (10, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "do_while_phi_node_refinement" =
  print_ssa_test {|let x = undefined;
do {
  if (x === 3) {
    continue;
  }
  x;
} while (x != null);
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`x`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}
        };
        (7, 9) to (7, 10) => {
          {refinement = 3; writes = (1, 4) to (1, 5): (`x`)},
          {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}
        };
        (8, 0) to (8, 1) => {
          {refinement = Not (Not (Maybe)); writes = {refinement = 3; writes = (1, 4) to (1, 5): (`x`)},{refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}}
        }] |}]

let%expect_test "for_no_init_no_update" =
  print_ssa_test {|let x = undefined;
for (;x != null;) {
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 6) to (2, 7) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "for_no_init_no_update_throw" =
  print_ssa_test {|let x = undefined;
for (;x != null;) {
  throw 'error';
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 6) to (2, 7) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "for_no_init_no_update_break_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (; x != null; ) {
  if (y == null) {
    break;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 7) to (3, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)}
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_no_init_no_update_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (;x != null;) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          (2, 4) to (2, 5): (`y`)
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "for_no_init_no_update_continue" =
  print_ssa_test {|let x = undefined;
for (; x != null; ) {
  continue;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 7) to (2, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "for_no_init_no_update_continue_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (;x != null;) {
  if (y == null) {
    continue;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)}
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "for_no_init_no_update_phi_refinement" =
  print_ssa_test {|let x = undefined;
for (; x != null; ) {
  if (x === 3) {
    break;
  }
  x;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 7) to (2, 8) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 6) to (3, 7) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (3); writes = {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_shadow" =
  print_ssa_test {|let x = undefined;
for (let x = null; x != null; x++) {
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 19) to (2, 20) => {
          (2, 9) to (2, 10): (`x`)
        };
        (2, 30) to (2, 31) => {
          {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)}
        };
        (4, 0) to (4, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for" =
  print_ssa_test {|for (let x = 3; x != null; x++) {
  x;
}|};
    [%expect {|
      [
        (1, 16) to (1, 17) => {
          (1, 9) to (1, 10): (`x`)
        };
        (1, 27) to (1, 28) => {
          {refinement = Not (Maybe); writes = (1, 9) to (1, 10): (`x`)}
        };
        (2, 2) to (2, 3) => {
          {refinement = Not (Maybe); writes = (1, 9) to (1, 10): (`x`)}
        }] |}]

let%expect_test "for_throw" =
  print_ssa_test {|for (let x = 3; x != null; x++) {
  throw 'error';
}|};
    [%expect {|
      [
        (1, 16) to (1, 17) => {
          (1, 9) to (1, 10): (`x`)
        };
        (1, 27) to (1, 28) => {
          unreachable
        }] |}]

let%expect_test "for_break_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let x = 3; x != null; x++) {
  if (y == null) {
    break;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 16) to (2, 17) => {
          (2, 9) to (2, 10): (`x`)
        };
        (2, 27) to (2, 28) => {
          {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)}
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "for_with_runtime_writes" =
  print_ssa_test {|let y = undefined;
for (let x = 3; x != null; x++) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 16) to (2, 17) => {
          (2, 9) to (2, 10): (`x`)
        };
        (2, 27) to (2, 28) => {
          (4, 4) to (4, 5): (`x`),
          {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)}
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          (1, 4) to (1, 5): (`y`)
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "for_continue" =
  print_ssa_test {|for (let x = 3; x != null; x++) {
  continue;
}
x;|};
    [%expect {|
      [
        (1, 16) to (1, 17) => {
          (1, 9) to (1, 10): (`x`)
        };
        (1, 27) to (1, 28) => {
          {refinement = Not (Maybe); writes = (1, 9) to (1, 10): (`x`)}
        };
        (4, 0) to (4, 1) => {
          Global x
        }] |}]

let%expect_test "for_continue_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let x = 3; x != null; x++) {
  if (y == null) {
    continue;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 16) to (2, 17) => {
          (2, 9) to (2, 10): (`x`)
        };
        (2, 27) to (2, 28) => {
          {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)}
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "no_havoc_before_write_seen" =
  print_ssa_test {|function f() { return 42 }
var x: number;
x;
x = 42;|};
  [%expect {|
    [
      (3, 0) to (3, 1) => {
        (uninitialized)
      }] |}]

let%expect_test "havoc_before_write_seen" =
  print_ssa_test {|function f() { return 42 }
f();
var x: number;
x;
x = 42;|};
  [%expect {|
    [
      (2, 0) to (2, 1) => {
        (1, 9) to (1, 10): (`f`)
      };
      (4, 0) to (4, 1) => {
        (uninitialized)
      }] |}]

let%expect_test "dont_havoc_to_uninit_in_function" =
  print_ssa_test {|function f() { return 42 }
function g() { return f() }|};
  [%expect {|
    [
      (2, 22) to (2, 23) => {
        (1, 9) to (1, 10): (`f`)
      }] |}]

let%expect_test "switch_decl" =
  print_ssa_test {|switch ('') { case '': const foo = ''; foo; };|};
  [%expect {|
    [
      (1, 39) to (1, 42) => {
        (1, 29) to (1, 32): (`foo`)
      }] |}]

let%expect_test "switch_weird_decl" =
  print_ssa_test {|switch ('') { case l: 0; break; case '': let l };|};
  [%expect {|
    [
      (1, 19) to (1, 20) => {
        (undeclared)
      }] |}]

let%expect_test "switch_shadow" =
  print_ssa_test {|function switch_scope(x) {
  switch (x) {
    default:
      let x;
      x = ""; // doesn't refine outer x
      x
  }
  x
}|};
  [%expect {|
    [
      (2, 10) to (2, 11) => {
        (1, 22) to (1, 23): (`x`)
      };
      (6, 6) to (6, 7) => {
        (5, 6) to (5, 7): (`x`)
      };
      (8, 2) to (8, 3) => {
        (1, 22) to (1, 23): (`x`)
      }] |}]

let%expect_test "switch_nested_block_shadow" =
  print_ssa_test {|function switch_scope() {
  switch ('foo') {
    case 'foo': {
      const bar = 3;
      break;
    }
  }
  bar;
  const {bar} = {};
  bar;
}|};
  [%expect {|
    [
      (8, 2) to (8, 5) => {
        (undeclared)
      };
      (10, 2) to (10, 5) => {
        (9, 9) to (9, 12): (`bar`)
      }] |}]

let%expect_test "for_nested_block_shadow" =
  print_ssa_test {|function for_scope() {
  for (;;) {
    const bar = 3;
    break;
  }
  bar;
  const {bar} = {};
  bar;
}|};
  [%expect {|
    [
      (6, 2) to (6, 5) => {
        (undeclared)
      };
      (8, 2) to (8, 5) => {
        (7, 9) to (7, 12): (`bar`)
      }] |}]

let%expect_test "for_in" =
  print_ssa_test {|let stuff = {}
for (let thing in stuff) {
  thing
}|};
    [%expect {|
      [
        (2, 18) to (2, 23) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 9) to (2, 14): (`thing`)
        }] |}]

let%expect_test "for_in_reassign" =
  print_ssa_test {|let stuff = {}
for (let thing in stuff) {
  thing;
  thing = 3;
}|};
    [%expect {|
      [
        (2, 18) to (2, 23) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 9) to (2, 14): (`thing`)
        }] |}]

let%expect_test "for_in_destructure" =
  print_ssa_test {|let stuff = {}
for (let {thing} in stuff) {
  thing;
}|};
    [%expect {|
      [
        (2, 20) to (2, 25) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 10) to (2, 15): (`thing`)
        }] |}]

let%expect_test "for_in_destructure_reassign" =
  print_ssa_test {|let stuff = {}
for (let {thing} in stuff) {
  thing;
  thing = 3;
}|};
    [%expect {|
      [
        (2, 20) to (2, 25) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 10) to (2, 15): (`thing`)
        }] |}]

let%expect_test "for_in_shadow" =
  print_ssa_test {|let thing = undefined;
let stuff = {}
for (let thing in stuff) {
  thing;
}
thing;|};
    [%expect {|
      [
        (1, 12) to (1, 21) => {
          Global undefined
        };
        (3, 18) to (3, 23) => {
          (2, 4) to (2, 9): (`stuff`)
        };
        (4, 2) to (4, 7) => {
          (3, 9) to (3, 14): (`thing`)
        };
        (6, 0) to (6, 5) => {
          (1, 4) to (1, 9): (`thing`)
        }] |}]

let%expect_test "for_in_throw" =
  print_ssa_test {|let x = undefined;
for (let thing in {}) {
  throw 'error';
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_in_break_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let thing in {}) {
  if (y == null) {
    break;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "for_in_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (let thing in {}) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          (2, 4) to (2, 5): (`y`)
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_in_continue" =
  print_ssa_test {|let x = undefined;
for (let thing in {}) {
  continue;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_in_continue_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let thing in {}) {
  if (y == null) {
    continue;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "for_in_reassign_right" =
  print_ssa_test {|let stuff = {};
for (let thing in stuff) {
  stuff = [];
}
stuff;|};
    [%expect {|
      [
        (2, 18) to (2, 23) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (5, 0) to (5, 5) => {
          (1, 4) to (1, 9): (`stuff`)
        }] |}]

let%expect_test "for_of" =
  print_ssa_test {|let stuff = {}
for (let thing of stuff) {
  thing
}|};
    [%expect {|
      [
        (2, 18) to (2, 23) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 9) to (2, 14): (`thing`)
        }] |}]

let%expect_test "for_of_reassign" =
  print_ssa_test {|let stuff = {}
for (let thing of stuff) {
  thing;
  thing = 3;
}|};
    [%expect {|
      [
        (2, 18) to (2, 23) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 9) to (2, 14): (`thing`)
        }] |}]

let%expect_test "for_of_destructure" =
  print_ssa_test {|let stuff = {}
for (let {thing} of stuff) {
  thing;
}|};
    [%expect {|
      [
        (2, 20) to (2, 25) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 10) to (2, 15): (`thing`)
        }] |}]

let%expect_test "for_of_destructure_reassign" =
  print_ssa_test {|let stuff = {}
for (let {thing} of stuff) {
  thing;
  thing = 3;
}|};
    [%expect {|
      [
        (2, 20) to (2, 25) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (3, 2) to (3, 7) => {
          (2, 10) to (2, 15): (`thing`)
        }] |}]

let%expect_test "for_of_shadow" =
  print_ssa_test {|let thing = undefined;
let stuff = {}
for (let thing of stuff) {
  thing;
}
thing;|};
    [%expect {|
      [
        (1, 12) to (1, 21) => {
          Global undefined
        };
        (3, 18) to (3, 23) => {
          (2, 4) to (2, 9): (`stuff`)
        };
        (4, 2) to (4, 7) => {
          (3, 9) to (3, 14): (`thing`)
        };
        (6, 0) to (6, 5) => {
          (1, 4) to (1, 9): (`thing`)
        }] |}]

let%expect_test "for_of_throw" =
  print_ssa_test {|let x = undefined;
for (let thing of {}) {
  throw 'error';
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_of_break_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let thing of {}) {
  if (y == null) {
    break;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "for_of_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (let thing of {}) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (4, 6) to (4, 7) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          (2, 4) to (2, 5): (`y`)
        };
        (9, 0) to (9, 1) => {
          (2, 4) to (2, 5): (`y`)
        };
        (10, 0) to (10, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_of_continue" =
  print_ssa_test {|let x = undefined;
for (let thing of {}) {
  continue;
}
x;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "for_of_continue_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let thing of {}) {
  if (y == null) {
    continue;
  }
  y;
}
y;|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 6) to (3, 7) => {
          (1, 4) to (1, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)}
        };
        (8, 0) to (8, 1) => {
          (1, 4) to (1, 5): (`y`)
        }] |}]

let%expect_test "for_of_reassign_right" =
  print_ssa_test {|let stuff = {};
for (let thing of stuff) {
  stuff = [];
}
stuff;|};
    [%expect {|
      [
        (2, 18) to (2, 23) => {
          (1, 4) to (1, 9): (`stuff`)
        };
        (5, 0) to (5, 5) => {
          (1, 4) to (1, 9): (`stuff`)
        }] |}]

let%expect_test "invariant" =
  print_ssa_test {|let x = undefined;
invariant(x != null, "other arg");
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 9) => {
          Global invariant
        };
        (2, 10) to (2, 11) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 0) to (3, 1) => {
          {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "invariant_false" =
  print_ssa_test {|let x = undefined;
if (x === 3) {
  invariant(false);
}
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 11) => {
          Global invariant
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "invariant_no_args" =
  print_ssa_test {|let x = undefined;
if (x === 3) {
  invariant();
}
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 4) to (2, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 2) to (3, 11) => {
          Global invariant
        };
        (5, 0) to (5, 1) => {
          {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}
        }] |}]

let%expect_test "invariant_reassign" =
  print_ssa_test {|let x = undefined;
if (true) {
  invariant(false, x = 3);
}
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (3, 2) to (3, 11) => {
          Global invariant
        };
        (5, 0) to (5, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "try_catch_invariant_reassign" =
  print_ssa_test {|let x = undefined;

try {
  if (true) {
    invariant(false, x = 3);
  }
} finally {
  x;
}|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (5, 4) to (5, 13) => {
          Global invariant
        };
        (8, 2) to (8, 3) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "switch_empty" =
  print_ssa_test {|let x = undefined;
switch (x) {};
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 0) to (2, 13) => {
          (1, 4) to (1, 5): (`x`)
        };
        (2, 8) to (2, 9) => {
          (1, 4) to (1, 5): (`x`)
        };
        (3, 0) to (3, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "switch_only_default" =
  print_ssa_test {|let x = undefined;
switch (x) {
  default: {
    x;
  }
};
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 9) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 4) to (4, 5) => {
          (1, 4) to (1, 5): (`x`)
        };
        (7, 0) to (7, 1) => {
          (1, 4) to (1, 5): (`x`)
        }] |}]

let%expect_test "switch_break_every_case" =
  print_ssa_test {|let x = undefined;
switch (x) {
  case null:
    x;
    break;
  case 3:
    x;
    break;
  case false:
    x;
    break;
  default: {
    x;
  }
};
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 9) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 4) to (4, 5) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)}
        };
        (7, 4) to (7, 5) => {
          {refinement = 3; writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (10, 4) to (10, 5) => {
          {refinement = false; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}
        };
        (13, 4) to (13, 5) => {
          {refinement = Not (false); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}
        };
        (16, 0) to (16, 1) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)},
          {refinement = 3; writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}},
          {refinement = false; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}},
          {refinement = Not (false); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}
        }]|}]

let%expect_test "switch_prop_return_every_case" =
  print_ssa_test {|function foo() {
let x = undefined;
switch (x) {
  case 1:
    return;
  case 2:
    return;
  default:
    return;
};
}
|};
    [%expect {|
      [
        (2, 8) to (2, 17) => {
          Global undefined
        };
        (3, 8) to (3, 9) => {
          (2, 4) to (2, 5): (`x`)
        }]
      |}]

let%expect_test "switch_with_fallthroughs" =
  print_ssa_test {|let x = undefined;
switch (x) {
  case null:
    x;
  case 3:
    x;
    break;
  case true:
  case false:
    x;
  default: {
    x;
  }
}
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 9) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 4) to (4, 5) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)}
        };
        (6, 4) to (6, 5) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)},
          {refinement = 3; writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (10, 4) to (10, 5) => {
          {refinement = true; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}},
          {refinement = false; writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}}
        };
        (12, 4) to (12, 5) => {
          {refinement = true; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}},
          {refinement = false; writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}},
          {refinement = Not (false); writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}}
        };
        (15, 0) to (15, 1) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)},
          {refinement = 3; writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}},
          {refinement = true; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}},
          {refinement = false; writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}},
          {refinement = Not (false); writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}}
        }] |}]

let%expect_test "switch_merge_all_breaks" =
  print_ssa_test {|let x;
switch (1) {
  case 1:
    x = 1;
    break;
  default:
    x = 2;
    break;
};
x;
|};
    [%expect {|
      [
        (10, 0) to (10, 1) => {
          (4, 4) to (4, 5): (`x`),
          (7, 4) to (7, 5): (`x`)
        }] |}]

let%expect_test "switch_throw_in_default" =
  print_ssa_test {|let x = undefined;
switch (x) {
  case null:
    x;
  case 3:
    x;
    break;
  case true:
  case false:
    x;
    break;
  default: {
    throw 'error'
  }
};
x;
|};
    [%expect {|
      [
        (1, 8) to (1, 17) => {
          Global undefined
        };
        (2, 8) to (2, 9) => {
          (1, 4) to (1, 5): (`x`)
        };
        (4, 4) to (4, 5) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)}
        };
        (6, 4) to (6, 5) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)},
          {refinement = 3; writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}
        };
        (10, 4) to (10, 5) => {
          {refinement = true; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}},
          {refinement = false; writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}}
        };
        (16, 0) to (16, 1) => {
          {refinement = Null; writes = (1, 4) to (1, 5): (`x`)},
          {refinement = 3; writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}},
          {refinement = true; writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}},
          {refinement = false; writes = {refinement = Not (true); writes = {refinement = Not (3); writes = {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)}}}}
        }] |}]

let%expect_test "arguments_eval_read" =
  print_ssa_test {|
arguments;
eval;
|};
    [%expect {|
      [
        (2, 0) to (2, 9) => {
          ModuleScoped arguments
        };
        (3, 0) to (3, 4) => {
          ModuleScoped eval
        }] |}]

let%expect_test "arguments_shadowed" =
  print_ssa_test {|
function foo(arguments) {
  arguments;
}
|};
    [%expect {|
      [
        (3, 2) to (3, 11) => {
          (2, 13) to (2, 22): (`arguments`)
        }] |}]

let%expect_test "global_refinement" =
  print_ssa_test {|
Map != null && Map
|};
    [%expect {|
      [
        (2, 0) to (2, 3) => {
          Global Map
        };
        (2, 15) to (2, 18) => {
          {refinement = Not (Maybe); writes = Global Map}
        }] |}]

let%expect_test "global_refinement_control_flow" =
  print_ssa_test {|
if (Map != null) {
  throw 'error';
}
Map;
|};
    [%expect {|
      [
        (2, 4) to (2, 7) => {
          Global Map
        };
        (5, 0) to (5, 3) => {
          {refinement = Not (Not (Maybe)); writes = Global Map}
        }] |}]

let%expect_test "global_overwrite" =
  print_ssa_test {|
if (true) {
  undefined = null;
}
undefined;
|};
    [%expect {|
      [
        (5, 0) to (5, 9) => {
          Global undefined,
          (3, 2) to (3, 11): (`undefined`)
        }] |}]

let%expect_test "havoc_from_uninitialized" =
  print_ssa_test {|
var x: number;
function havoc() {
    x = 42;
}
havoc();
(x: void)
// todo: this should probably also include undefined
|};
  [%expect {|
    [
      (6, 0) to (6, 5) => {
        (3, 9) to (3, 14): (`havoc`)
      };
      (7, 1) to (7, 2) => {
        (uninitialized),
        (2, 4) to (2, 5): (`x`)
      }] |}]

let%expect_test "captured_havoc" =
  print_ssa_test {|function g() {
  var xx : { p : number } | null = { p : 4 };
  if (xx) {
    return function () {
       xx.p = 3;
    }
  }
}
|};
  [%expect {|
    [
      (3, 6) to (3, 8) => {
        (2, 6) to (2, 8): (`xx`)
      };
      (5, 7) to (5, 9) => {
        {refinement = Truthy; writes = (2, 6) to (2, 8): (`xx`)}
      }] |}]

let%expect_test "no_providers" =
  print_ssa_test {|
var x;
function fn() {
    x;
}
|};
  [%expect {|
    [
      (4, 4) to (4, 5) => {
        (uninitialized)
      }] |}]

let%expect_test "class_expr" =
  print_ssa_test {|
let y = 42;
let x = class y { m() { x; y } };
y;
|};
    [%expect {|
      [
        (3, 24) to (3, 25) => {
          (3, 4) to (3, 5): (`x`)
        };
        (3, 27) to (3, 28) => {
          (3, 14) to (3, 15): (`y`)
        };
        (4, 0) to (4, 1) => {
          (2, 4) to (2, 5): (`y`)
        }]|}]

let%expect_test "havoc" =
  print_ssa_test {|
let x = 3;
function f() { x = 'string'}
let y = 3;
if (x != null && y != null) {
  f();
  x;
  y;
}
|};
    [%expect {|
      [
        (5, 4) to (5, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 17) to (5, 18) => {
          (4, 4) to (4, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          (3, 9) to (3, 10): (`f`)
        };
        (7, 2) to (7, 3) => {
          (2, 4) to (2, 5): (`x`)
        };
        (8, 2) to (8, 3) => {
          {refinement = Not (Maybe); writes = (4, 4) to (4, 5): (`y`)}
        }] |}]

let%expect_test "provider_closure_havoc_1" =
  print_ssa_test {|
var x = null;
function havoc() { x = 21 }

if (typeof x === 'number') {
  x;
  havoc();
  x;
}
|};
    [%expect {|
      [
        (5, 11) to (5, 12) => {
          (2, 4) to (2, 5): (`x`)
        };
        (6, 2) to (6, 3) => {
          {refinement = number; writes = (2, 4) to (2, 5): (`x`)}
        };
        (7, 2) to (7, 7) => {
          (3, 9) to (3, 14): (`havoc`)
        };
        (8, 2) to (8, 3) => {
          (3, 19) to (3, 20): (`x`),
          {refinement = number; writes = (2, 4) to (2, 5): (`x`)}
        }]
      |}]

let%expect_test "provider_closure_havoc_2" =
  print_ssa_test {|
var y = null;
function havoc() { y = 31 }
function havoc2() { y = 42 } // a non provider write, so no special treatment.

if (typeof y === 'number') {
  y;
  havoc();
  y; // should be fully havoced
}
|};
    [%expect {|
      [
        (6, 11) to (6, 12) => {
          (2, 4) to (2, 5): (`y`)
        };
        (7, 2) to (7, 3) => {
          {refinement = number; writes = (2, 4) to (2, 5): (`y`)}
        };
        (8, 2) to (8, 7) => {
          (3, 9) to (3, 14): (`havoc`)
        };
        (9, 2) to (9, 3) => {
          (2, 4) to (2, 5): (`y`),
          (3, 19) to (3, 20): (`y`)
        }]
      |}]

let%expect_test "provider_closure_havoc_3" =
  print_ssa_test {|
var z = null;
function havocz() {
  z = 42;
}

havocz();
z;

if (typeof z === 'number'){
  havocz();
  z;
}
|};
    [%expect {|
      [
        (7, 0) to (7, 6) => {
          (3, 9) to (3, 15): (`havocz`)
        };
        (8, 0) to (8, 1) => {
          (2, 4) to (2, 5): (`z`),
          (4, 2) to (4, 3): (`z`)
        };
        (10, 11) to (10, 12) => {
          (2, 4) to (2, 5): (`z`),
          (4, 2) to (4, 3): (`z`)
        };
        (11, 2) to (11, 8) => {
          (3, 9) to (3, 15): (`havocz`)
        };
        (12, 2) to (12, 3) => {
          (4, 2) to (4, 3): (`z`),
          {refinement = number; writes = (2, 4) to (2, 5): (`z`),(4, 2) to (4, 3): (`z`)}
        }]

      |}]

let%expect_test "latent_refinements" =
  print_ssa_test {|
let x = 3;
function f() { x = 'string'}
let y = 3;
if (f(x, y)) {
  x;
  y;
}
if (y.f(x, y)) {
  // No refinements on either
  x;
  y;
}
|};
    [%expect {|
      [
        (5, 4) to (5, 5) => {
          (3, 9) to (3, 10): (`f`)
        };
        (5, 6) to (5, 7) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 9) to (5, 10) => {
          (4, 4) to (4, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          {refinement = LatentR (index = 0); writes = (2, 4) to (2, 5): (`x`)}
        };
        (7, 2) to (7, 3) => {
          {refinement = LatentR (index = 1); writes = (4, 4) to (4, 5): (`y`)}
        };
        (9, 4) to (9, 5) => {
          (4, 4) to (4, 5): (`y`)
        };
        (9, 8) to (9, 9) => {
          (2, 4) to (2, 5): (`x`)
        };
        (9, 11) to (9, 12) => {
          (4, 4) to (4, 5): (`y`)
        };
        (11, 2) to (11, 3) => {
          {refinement = LatentR (index = 0); writes = (2, 4) to (2, 5): (`x`)}
        };
        (12, 2) to (12, 3) => {
          {refinement = LatentR (index = 1); writes = (4, 4) to (4, 5): (`y`)}
        }] |}]

let%expect_test "loop read from havoc" =
  print_ssa_test {|
type T = string;

function switch_fn() {
  declare const n: number;

  switch (n) {
    case 2:
      ('a': T);
      return 1;

    case 3:
      if (String(('a': T))) {}
  }
}

  |};
    [%expect {|
      [
        (7, 2) to (14, 3) => {
          {refinement = Not (3); writes = {refinement = Not (2); writes = (5, 16) to (5, 17): (`n`)}}
        };
        (7, 10) to (7, 11) => {
          (5, 16) to (5, 17): (`n`)
        };
        (9, 12) to (9, 13) => {
          (2, 5) to (2, 6): (`T`)
        };
        (13, 10) to (13, 16) => {
          Global String
        };
        (13, 23) to (13, 24) => {
          (2, 5) to (2, 6): (`T`)
        }] |}]

let%expect_test "read merged value instead of captured value" =
  print_ssa_test {|
let a: string | number = '';

const f = () => {
  if (true) {
    a = 1;
  } else {
    a = 2;
  }
  a;
}
  |};
    [%expect {|
      [
        (10, 2) to (10, 3) => {
          (6, 4) to (6, 5): (`a`),
          (8, 4) to (8, 5): (`a`)
        }] |}]

let%expect_test "loop read from havoc" =
  print_ssa_test {|
let data: any = [];

function asObjectList(length: number) {
  while (data) {
    data = data.concat(data);
  }
}
  |};
    [%expect {|
      [
        (5, 9) to (5, 13) => {
          (2, 4) to (2, 8): (`data`)
        };
        (6, 11) to (6, 15) => {
          {refinement = Truthy; writes = (2, 4) to (2, 8): (`data`)}
        };
        (6, 23) to (6, 27) => {
          {refinement = Truthy; writes = (2, 4) to (2, 8): (`data`)}
        }] |}]

let%expect_test "heap_refinement_basic" =
  print_ssa_test {|
let x = {};
if (x.foo === 3) {
  x.foo;
  if (x.foo === 4) {
    x.foo;
  }
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 2) to (4, 3) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 2) to (4, 7) => {
          {refinement = 3; writes = projection at (3, 4) to (3, 9)}
        };
        (5, 6) to (5, 7) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (5, 6) to (5, 11) => {
          {refinement = 3; writes = projection at (3, 4) to (3, 9)}
        };
        (6, 4) to (6, 5) => {
          {refinement = SentinelR foo; writes = {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}}
        };
        (6, 4) to (6, 9) => {
          {refinement = 4; writes = {refinement = 3; writes = projection at (3, 4) to (3, 9)}}
        }] |}]

let%expect_test "heap_refinement_this_basic" =
  print_ssa_test {|
if (this.foo === 3) {
  this.foo;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 8) => {
          This(global)
        };
        (3, 2) to (3, 6) => {
          {refinement = SentinelR foo; writes = This(global)}
        };
        (3, 2) to (3, 10) => {
          {refinement = 3; writes = projection at (2, 4) to (2, 12)}
        }]
     |}]

let%expect_test "heap_refinement_super" =
  print_ssa_test {|
class A {
  static foo() {
    if (super.baz === 3) {
      super.baz;
    }
  }
  bar() {
    if (super.baz === 3) {
      super.baz;
    }
  }
}

|};
    [%expect {|
      [
        (4, 8) to (4, 13) => {
          Super(static)
        };
        (5, 6) to (5, 11) => {
          {refinement = SentinelR baz; writes = Super(static)}
        };
        (5, 6) to (5, 15) => {
          {refinement = 3; writes = projection at (4, 8) to (4, 17)}
        };
        (9, 8) to (9, 13) => {
          Super(instance)
        };
        (10, 6) to (10, 11) => {
          {refinement = SentinelR baz; writes = Super(instance)}
        };
        (10, 6) to (10, 15) => {
          {refinement = 3; writes = projection at (9, 8) to (9, 17)}
        }]
     |}]

let%expect_test "heap_refinement_basic" =
  print_ssa_test {|
let x = {};
if (x.foo === 3) {
  x.foo;
  if (x.foo === 4) {
    x.foo;
  }
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 2) to (4, 3) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 2) to (4, 7) => {
          {refinement = 3; writes = projection at (3, 4) to (3, 9)}
        };
        (5, 6) to (5, 7) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (5, 6) to (5, 11) => {
          {refinement = 3; writes = projection at (3, 4) to (3, 9)}
        };
        (6, 4) to (6, 5) => {
          {refinement = SentinelR foo; writes = {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}}
        };
        (6, 4) to (6, 9) => {
          {refinement = 4; writes = {refinement = 3; writes = projection at (3, 4) to (3, 9)}}
        }] |}]

let%expect_test "heap_refinement_destrucure" =
  print_ssa_test {|
let x = {};
if (x.foo === 3) {
  const { foo } = x;
  foo;
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 10) to (4, 13) => {
          {refinement = 3; writes = projection at (3, 4) to (3, 9)}
        };
        (4, 18) to (4, 19) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (5, 2) to (5, 5) => {
          {refinement = 3; writes = (4, 10) to (4, 13): (`foo`)}
        }] |}]

let%expect_test "heap_refinement_from_assign_destrucure" =
  print_ssa_test {|
let x = {};
x.foo = 3;
const { foo } = x;
foo;
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 8) to (4, 11) => {
          (3, 0) to (3, 5): (some property)
        };
        (4, 16) to (4, 17) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 0) to (5, 3) => {
          (4, 8) to (4, 11): (`foo`)
        }]
      |}]

let%expect_test "heap_refinement_deep_destrucure" =
  print_ssa_test {|
let x = {};
if (x.bar.baz.hello.world === 4) {
  const { bar: { baz: { hello: {world: hi} } } } = x;
  hi;
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 24) to (4, 29) => {
          {refinement = SentinelR world; writes = projection at (3, 4) to (3, 19)}
        };
        (4, 32) to (4, 37) => {
          {refinement = 4; writes = projection at (3, 4) to (3, 25)}
        };
        (4, 51) to (4, 52) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 2) to (5, 4) => {
          {refinement = 4; writes = (4, 39) to (4, 41): (`hi`)}
        }] |}]

let%expect_test "heap_refinement_from_assign_deep_destrucure" =
  print_ssa_test {|
let x = {};
x.bar.baz.hello.world = 4;
const { bar: { baz: { hello: {world: hi} } } } = x;
hi;
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 30) to (4, 35) => {
          (3, 0) to (3, 21): (some property)
        };
        (4, 49) to (4, 50) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 0) to (5, 2) => {
          (4, 37) to (4, 39): (`hi`)
        }] |}]

let%expect_test "heap_refinement_merge_branches" =
  print_ssa_test {|
declare var invariant: any;
let x = {};
if (true) {
  invariant(x.foo === 3);
} else {
  invariant(x.foo === 4);
}
x.foo; // 3 | 4
|};
    [%expect {|
      [
        (5, 2) to (5, 11) => {
          (2, 12) to (2, 21): (`invariant`)
        };
        (5, 12) to (5, 13) => {
          (3, 4) to (3, 5): (`x`)
        };
        (7, 2) to (7, 11) => {
          (2, 12) to (2, 21): (`invariant`)
        };
        (7, 12) to (7, 13) => {
          (3, 4) to (3, 5): (`x`)
        };
        (9, 0) to (9, 1) => {
          {refinement = SentinelR foo; writes = (3, 4) to (3, 5): (`x`)},
          {refinement = SentinelR foo; writes = (3, 4) to (3, 5): (`x`)}
        };
        (9, 0) to (9, 5) => {
          {refinement = 3; writes = projection at (5, 12) to (5, 17)},
          {refinement = 4; writes = projection at (7, 12) to (7, 17)}
        }] |}]

let%expect_test "heap_refinement_normalize_from_and" =
  print_ssa_test {|
let x = {};
if (x.foo === 3 && x.foo === 3) {
  x.foo; // 3
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (3, 19) to (3, 20) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (3, 19) to (3, 24) => {
          {refinement = 3; writes = projection at (3, 4) to (3, 9)}
        };
        (4, 2) to (4, 3) => {
          {refinement = And (SentinelR foo, SentinelR foo); writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 2) to (4, 7) => {
          {refinement = And (3, 3); writes = projection at (3, 4) to (3, 9)}
        }]
       |}]

let%expect_test "heap_refinement_normalize_from_or" =
  print_ssa_test {|
let x = {};
if (x.foo === 3 || x.foo === 4) {
  x.foo; // 3 | 4
}
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 4) to (2, 5): (`x`)
        };
        (3, 19) to (3, 20) => {
          {refinement = Not (SentinelR foo); writes = (2, 4) to (2, 5): (`x`)}
        };
        (3, 19) to (3, 24) => {
          {refinement = Not (3); writes = projection at (3, 4) to (3, 9)}
        };
        (4, 2) to (4, 3) => {
          {refinement = Or (SentinelR foo, SentinelR foo); writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 2) to (4, 7) => {
          {refinement = Or (3, 4); writes = projection at (3, 4) to (3, 9)}
        }]
       |}]

let%expect_test "heap_refinement_one_branch" =
  print_ssa_test {|
declare var invariant: any;
let x = {};
if (true) {
  invariant(x.foo === 3);
} else {
}
x.foo; // No refinement
|};
    [%expect {|
      [
        (5, 2) to (5, 11) => {
          (2, 12) to (2, 21): (`invariant`)
        };
        (5, 12) to (5, 13) => {
          (3, 4) to (3, 5): (`x`)
        };
        (8, 0) to (8, 1) => {
          (3, 4) to (3, 5): (`x`)
        }] |}]

let%expect_test "heap_refinement_while_loop_subject_changed" =
  print_ssa_test {|
let x = {};
while (x.foo === 3) {
  x.foo;
  x = {};
  break;
}
x.foo; // No heap refinement here
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 2) to (4, 3) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 2) to (4, 7) => {
          {refinement = 3; writes = projection at (3, 7) to (3, 12)}
        };
        (8, 0) to (8, 1) => {
          (2, 4) to (2, 5): (`x`)
        }] |}]

let%expect_test "heap_refinement_while_loop_projection_changed" =
  print_ssa_test {|
declare var invariant: any;
let x = {};
while (x.foo === 3) {
  invariant(x.foo === 4);
  x.foo;
  break;
}
x.foo;
|};
    [%expect {|
      [
        (4, 7) to (4, 8) => {
          (3, 4) to (3, 5): (`x`)
        };
        (5, 2) to (5, 11) => {
          (2, 12) to (2, 21): (`invariant`)
        };
        (5, 12) to (5, 13) => {
          {refinement = SentinelR foo; writes = (3, 4) to (3, 5): (`x`)}
        };
        (5, 12) to (5, 17) => {
          {refinement = 3; writes = projection at (4, 7) to (4, 12)}
        };
        (6, 2) to (6, 3) => {
          {refinement = SentinelR foo; writes = {refinement = SentinelR foo; writes = (3, 4) to (3, 5): (`x`)}}
        };
        (6, 2) to (6, 7) => {
          {refinement = 4; writes = {refinement = 3; writes = projection at (4, 7) to (4, 12)}}
        };
        (9, 0) to (9, 1) => {
          (3, 4) to (3, 5): (`x`)
        }] |}]

let%expect_test "heap_refinement_while_loop_negated" =
  print_ssa_test {|
let x = {};
while (x.foo === 3) {
  x.foo;
}
x.foo;
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 2) to (4, 3) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 2) to (4, 7) => {
          {refinement = 3; writes = projection at (3, 7) to (3, 12)}
        };
        (6, 0) to (6, 1) => {
          {refinement = Not (SentinelR foo); writes = (2, 4) to (2, 5): (`x`)}
        };
        (6, 0) to (6, 5) => {
          {refinement = Not (3); writes = projection at (3, 7) to (3, 12)}
        }] |}]

let%expect_test "heap_refinement_loop_control_flow_write" =
  print_ssa_test {|
let x = {};
while (x.foo === 3) {
  if (x.foo === 4) {
    x.foo;
  } else { throw 'error'}
  x.foo;
}
x.foo;
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 6) to (4, 7) => {
          {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 6) to (4, 11) => {
          {refinement = 3; writes = projection at (3, 7) to (3, 12)}
        };
        (5, 4) to (5, 5) => {
          {refinement = SentinelR foo; writes = {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}}
        };
        (5, 4) to (5, 9) => {
          {refinement = 4; writes = {refinement = 3; writes = projection at (3, 7) to (3, 12)}}
        };
        (7, 2) to (7, 3) => {
          {refinement = SentinelR foo; writes = {refinement = SentinelR foo; writes = (2, 4) to (2, 5): (`x`)}}
        };
        (7, 2) to (7, 7) => {
          {refinement = 4; writes = {refinement = 3; writes = projection at (3, 7) to (3, 12)}}
        };
        (9, 0) to (9, 1) => {
          {refinement = Not (SentinelR foo); writes = (2, 4) to (2, 5): (`x`)}
        };
        (9, 0) to (9, 5) => {
          {refinement = Not (3); writes = projection at (3, 7) to (3, 12),{refinement = 4; writes = projection at (3, 7) to (3, 12)}}
        }] |}]

let%expect_test "heap_refinement_write" =
  print_ssa_test {|
let x = {};
x.foo = 3;
x.foo;
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 0) to (4, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 0) to (4, 5) => {
          (3, 0) to (3, 5): (some property)
        }] |}]

let%expect_test "heap_refinement_write_havoc_member" =
  print_ssa_test {|
let x = {};
x.foo = 3;
let y = x;
y.foo = 3;
x.foo; // should no longer be refined
y.foo; // still refined
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 8) to (4, 9) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          (4, 4) to (4, 5): (`y`)
        };
        (6, 0) to (6, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (7, 0) to (7, 1) => {
          (4, 4) to (4, 5): (`y`)
        };
        (7, 0) to (7, 5) => {
          (5, 0) to (5, 5): (some property)
        };
        (6, 0) to (6, 5) => invalidated refinement by {
          property assignment at (5, 0) to (5, 9)
        }] |}]

let%expect_test "heap_refinement_write_havoc_elem" =
  print_ssa_test {|
let x = {};
x.foo = 3;
let y = x;
y.foo = 3;
y[x] = 4;
x.foo; // should no longer be refined
y.foo; // should no longer be refined
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 8) to (4, 9) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 0) to (5, 1) => {
          (4, 4) to (4, 5): (`y`)
        };
        (6, 0) to (6, 1) => {
          (4, 4) to (4, 5): (`y`)
        };
        (6, 2) to (6, 3) => {
          (2, 4) to (2, 5): (`x`)
        };
        (7, 0) to (7, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (8, 0) to (8, 1) => {
          (4, 4) to (4, 5): (`y`)
        };
        (7, 0) to (7, 5) => invalidated refinement by {
          property assignment at (6, 0) to (6, 8)
        };
        (8, 0) to (8, 5) => invalidated refinement by {
          property assignment at (6, 0) to (6, 8)
        }] |}]

let%expect_test "heap_refinement_havoc_on_write" =
  print_ssa_test {|
let x = {};
x.foo = 3;
x.foo;
x = {};
x.foo; // No more refinement
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 0) to (4, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 0) to (4, 5) => {
          (3, 0) to (3, 5): (some property)
        };
        (6, 0) to (6, 1) => {
          (5, 0) to (5, 1): (`x`)
        }] |}]

let%expect_test "unreachable_code" =
  print_ssa_test {|
x = 4;
x;
throw new Error();
var x = 3;
x;
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 0) to (2, 1): (`x`)
        };
        (4, 10) to (4, 15) => {
          Global Error
        };
        (6, 0) to (6, 1) => {
          unreachable
        }] |}]

let%expect_test "function_hoisted_typeof" =
  print_ssa_test {|
let x = null;
if (Math.random()) {
  x = 'string';
} else {
  x = 4;
}

x = 5;
// Note that the typeofs here report all the providers and not x = 5
function f<T: typeof x = typeof x>(y: typeof x): typeof x { return null; }
declare function f<T: typeof x = typeof x>(y: typeof x): typeof x;
// The return here should not be hoisted, but the param is because
// we havoc before we visit the params.
let y = (z: typeof x): typeof x => 3;
|};
    [%expect {|
      [
        (3, 4) to (3, 8) => {
          Global Math
        };
        (11, 21) to (11, 22) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (11, 32) to (11, 33) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (11, 45) to (11, 46) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (11, 56) to (11, 57) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (12, 29) to (12, 30) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (12, 40) to (12, 41) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (12, 43) to (12, 44) => {
          (15, 4) to (15, 5): (`y`)
        };
        (12, 53) to (12, 54) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (12, 64) to (12, 65) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (15, 19) to (15, 20) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        };
        (15, 30) to (15, 31) => {
          (2, 4) to (2, 5): (`x`),
          (4, 2) to (4, 3): (`x`),
          (6, 2) to (6, 3): (`x`)
        }] |}]

let%expect_test "hoisted_global_refinement" =
  print_ssa_test {|
if (global != null) {
  global;
  function f(x: typeof global) { }
}
|};
    [%expect {|
      [
        (2, 4) to (2, 10) => {
          Global global
        };
        (3, 2) to (3, 8) => {
          {refinement = Not (Maybe); writes = Global global}
        };
        (4, 23) to (4, 29) => {
          Global global
        }] |}]

let%expect_test "this_in_object" =
  print_ssa_test {|
({
  foo() {
    this;
  },
  get bar() {
    this;
  },
  set baz(f) {
    this;
  }
});
|};
    [%expect {|
      [
        (4, 4) to (4, 8) => {
          This(illegal)
        };
        (7, 4) to (7, 8) => {
          This(illegal)
        };
        (10, 4) to (10, 8) => {
          This(illegal)
        }]
     |}]

let%expect_test "type_alias" =
  print_ssa_test {|
type t = number;
let x: t = 42;
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 5) to (2, 6): (`t`)
        }] |}]

let%expect_test "type_alias_global" =
  print_ssa_test {|
let x: t = 42;
|};
    [%expect {|
      [
        (2, 7) to (2, 8) => {
          Global t
        }] |}]

let%expect_test "type_alias_refine" =
  print_ssa_test {|
if (t) {
  let x: t = 42;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global t
        };
        (3, 9) to (3, 10) => {
          {refinement = Truthy; writes = Global t}
        }] |}]

let%expect_test "type_alias_no_init" =
  print_ssa_test {|
type t = number;
let x: t;
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 5) to (2, 6): (`t`)
        }] |}]

let%expect_test "type_alias_lookup" =
  print_ssa_test {|
import * as React from 'react';
type T = React.ComponentType;
var C: React.ComponentType;
|};
    [%expect {|
      [
        (3, 9) to (3, 14) => {
          (2, 12) to (2, 17): (`React`)
        };
        (4, 7) to (4, 12) => {
          (2, 12) to (2, 17): (`React`)
        }] |}]

let%expect_test "conditional_type_with_infer" =
  print_ssa_test {|
// TS behavior: https://www.typescriptlang.org/play?#code/C4TwDgpgBAYg9nKBeKBLAdgMwgJygQSggA9gJ0ATAZygAoNs8AhI086tLXKAYSgH5OjKABEoALiHcAogEoBUANr4ANExU8VIldIC6EpavWbtegNxA
type Foo = infer A
    extends (infer B extends infer C ? infer D : infer E)
    ? [A,B,C,D,E]
    : [A,B,C,D,E];
|};
    [%expect {|
      [
        (5, 7) to (5, 8) => {
          Global A
        };
        (5, 9) to (5, 10) => {
          (4, 19) to (4, 20): (`B`)
        };
        (5, 11) to (5, 12) => {
          Global C
        };
        (5, 13) to (5, 14) => {
          (4, 45) to (4, 46): (`D`)
        };
        (5, 15) to (5, 16) => {
          (4, 55) to (4, 56): (`E`)
        };
        (6, 7) to (6, 8) => {
          Global A
        };
        (6, 9) to (6, 10) => {
          Global B
        };
        (6, 11) to (6, 12) => {
          Global C
        };
        (6, 13) to (6, 14) => {
          Global D
        };
        (6, 15) to (6, 16) => {
          Global E
        }] |}]

let%expect_test "no_cyclic_infer" =
print_ssa_test {|
// TS behavior: https://www.typescriptlang.org/play?#code/C4TwDgpgBAYg9nKBeKBnYAnAlgOwOZQQAewEOAJqlAN5QCGAXFLgGYQZQCChJZlUAIQDcUAEZNW7QT1IUq3AL5QA-Gky4CTHAFcAtqPZCAUEA
type Foo = string extends { a: infer A extends B, b: infer B extends A } ? string : number;
|};[%expect {|
      [
        (3, 47) to (3, 48) => {
          Global B
        };
        (3, 69) to (3, 70) => {
          Global A
        }] |}]

let%expect_test "new_type_arg" =
  print_ssa_test {|
type A = number;
new Set<A>();
|};
    [%expect {|
      [
        (3, 4) to (3, 7) => {
          Global Set
        };
        (3, 8) to (3, 9) => {
          (2, 5) to (2, 6): (`A`)
        }] |}]

let%expect_test "class_as_type" =
  print_ssa_test {|
class C { }
var x: C = new C();
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 6) to (2, 7): (`C`)
        };
        (3, 15) to (3, 16) => {
          (2, 6) to (2, 7): (`C`)
        }] |}]

let%expect_test "interface_as_type" =
  print_ssa_test {|
interface C { }
var x: C;
|};
    [%expect {|
      [
        (3, 7) to (3, 8) => {
          (2, 10) to (2, 11): (`C`)
        }] |}]

let%expect_test "hoist_type" =
  print_ssa_test {|
var x: T;
type T = number;
|};
    [%expect {|
      [
        (2, 7) to (2, 8) => {
          (3, 5) to (3, 6): (`T`)
        }] |}]

let%expect_test "hoist_interface" =
  print_ssa_test {|
var x: C;
interface C { }
|};
    [%expect {|
      [
        (2, 7) to (2, 8) => {
          (3, 10) to (3, 11): (`C`)
        }] |}]

let%expect_test "opaque" =
  print_ssa_test {|
var x: T;
opaque type T: C = C;
interface C { }
|};
    [%expect {|
      [
        (2, 7) to (2, 8) => {
          (3, 12) to (3, 13): (`T`)
        };
        (3, 15) to (3, 16) => {
          (4, 10) to (4, 11): (`C`)
        };
        (3, 19) to (3, 20) => {
          (4, 10) to (4, 11): (`C`)
        }] |}]

let%expect_test "mutual" =
  print_ssa_test {|
type A = Array<B>;
type B = { a: A };
|};
    [%expect {|
      [
        (2, 9) to (2, 14) => {
          Global Array
        };
        (2, 15) to (2, 16) => {
          (3, 5) to (3, 6): (`B`)
        };
        (3, 14) to (3, 15) => {
          (2, 5) to (2, 6): (`A`)
        }] |}]

let%expect_test "fun_tparam" =
  print_ssa_test {|
function f<X, Y: X = number>(x: X, y: Y) {
  (x: Y);
}
|};
    [%expect {|
      [
        (2, 17) to (2, 18) => {
          (2, 11) to (2, 12): (`X`)
        };
        (2, 32) to (2, 33) => {
          (2, 11) to (2, 12): (`X`)
        };
        (2, 38) to (2, 39) => {
          (2, 14) to (2, 15): (`Y`)
        };
        (3, 3) to (3, 4) => {
          (2, 29) to (2, 30): (`x`)
        };
        (3, 6) to (3, 7) => {
          (2, 14) to (2, 15): (`Y`)
        }] |}]

let%expect_test "fun_tparam_return" =
  print_ssa_test {|
function f<X, Y: X = number>(x: X, y: Y): Y {
  (x: Y);
}
|};
    [%expect {|
      [
        (2, 17) to (2, 18) => {
          (2, 11) to (2, 12): (`X`)
        };
        (2, 32) to (2, 33) => {
          (2, 11) to (2, 12): (`X`)
        };
        (2, 38) to (2, 39) => {
          (2, 14) to (2, 15): (`Y`)
        };
        (2, 42) to (2, 43) => {
          (2, 14) to (2, 15): (`Y`)
        };
        (3, 3) to (3, 4) => {
          (2, 29) to (2, 30): (`x`)
        };
        (3, 6) to (3, 7) => {
          (2, 14) to (2, 15): (`Y`)
        }] |}]

let%expect_test "fun_tparam_global_bound" =
  print_ssa_test {|
function f<Z: Z>() { }
|};
    [%expect {|
      [
        (2, 14) to (2, 15) => {
          Global Z
        }] |}]

let%expect_test "fun_inline_tparam" =
  print_ssa_test {|
let x = function f<X, Y: X = number>(x: X, y: Y) {
  (x: Y);
}
|};
    [%expect {|
      [
        (2, 25) to (2, 26) => {
          (2, 19) to (2, 20): (`X`)
        };
        (2, 40) to (2, 41) => {
          (2, 19) to (2, 20): (`X`)
        };
        (2, 46) to (2, 47) => {
          (2, 22) to (2, 23): (`Y`)
        };
        (3, 3) to (3, 4) => {
          (2, 37) to (2, 38): (`x`)
        };
        (3, 6) to (3, 7) => {
          (2, 22) to (2, 23): (`Y`)
        }] |}]

let%expect_test "arrow_fun_tparam" =
  print_ssa_test {|
let x = <X, Y: X = number>(x: X, y: Y) => {
  (x: Y);
}
|};
    [%expect {|
      [
        (2, 15) to (2, 16) => {
          (2, 9) to (2, 10): (`X`)
        };
        (2, 30) to (2, 31) => {
          (2, 9) to (2, 10): (`X`)
        };
        (2, 36) to (2, 37) => {
          (2, 12) to (2, 13): (`Y`)
        };
        (3, 3) to (3, 4) => {
          (2, 27) to (2, 28): (`x`)
        };
        (3, 6) to (3, 7) => {
          (2, 12) to (2, 13): (`Y`)
        }] |}]

let%expect_test "type_tparam" =
  print_ssa_test {|
type T<X, Y: X = number> = Array<[X, Y]>
|};
    [%expect {|
      [
        (2, 13) to (2, 14) => {
          (2, 7) to (2, 8): (`X`)
        };
        (2, 27) to (2, 32) => {
          Global Array
        };
        (2, 34) to (2, 35) => {
          (2, 7) to (2, 8): (`X`)
        };
        (2, 37) to (2, 38) => {
          (2, 10) to (2, 11): (`Y`)
        }] |}]

let%expect_test "opaque_tparam" =
  print_ssa_test {|
opaque type T<X>: X = X
|};
    [%expect {|
      [
        (2, 18) to (2, 19) => {
          (2, 14) to (2, 15): (`X`)
        };
        (2, 22) to (2, 23) => {
          (2, 14) to (2, 15): (`X`)
        }] |}]

let%expect_test "opaque_tparam" =
  print_ssa_test {|
declare opaque type T<X>: X;
|};
    [%expect {|
      [
        (2, 26) to (2, 27) => {
          (2, 22) to (2, 23): (`X`)
        }] |}]

let%expect_test "interface_tparam" =
  print_ssa_test {|
interface T<X, Y:X> {
  x: X;
  y: Y;
};
|};
    [%expect {|
      [
        (2, 17) to (2, 18) => {
          (2, 12) to (2, 13): (`X`)
        };
        (3, 5) to (3, 6) => {
          (2, 12) to (2, 13): (`X`)
        };
        (4, 5) to (4, 6) => {
          (2, 15) to (2, 16): (`Y`)
        }] |}]

let%expect_test "interface_miss" =
  print_ssa_test {|
interface T<X> extends X { };
|};
    [%expect {|
      [
        (2, 23) to (2, 24) => {
          Global X
        }] |}]

let%expect_test "interface_extends_tparam" =
  print_ssa_test {|
type X<S> = S;

interface T<X> extends X<X> {
  x: X
};
|};
    [%expect {|
      [
        (2, 12) to (2, 13) => {
          (2, 7) to (2, 8): (`S`)
        };
        (4, 23) to (4, 24) => {
          (2, 5) to (2, 6): (`X`)
        };
        (4, 25) to (4, 26) => {
          (4, 12) to (4, 13): (`X`)
        };
        (5, 5) to (5, 6) => {
          (4, 12) to (4, 13): (`X`)
        }] |}]

let%expect_test "fun_type_tparam" =
  print_ssa_test {|
type T<W> = <X: W, Y: X>(X) => Y
|};
    [%expect {|
      [
        (2, 16) to (2, 17) => {
          (2, 7) to (2, 8): (`W`)
        };
        (2, 22) to (2, 23) => {
          (2, 13) to (2, 14): (`X`)
        };
        (2, 25) to (2, 26) => {
          (2, 13) to (2, 14): (`X`)
        };
        (2, 31) to (2, 32) => {
          (2, 19) to (2, 20): (`Y`)
        }] |}]

let%expect_test "class_tparam" =
  print_ssa_test {|
class C<X, Y:X> extends X<X> implements Y<Y> {
  f<Z:Y>(x:X, y:Y) {
    let z: Z;
  }
}
|};
    [%expect {|
      [
        (2, 13) to (2, 14) => {
          (2, 8) to (2, 9): (`X`)
        };
        (2, 24) to (2, 25) => {
          Global X
        };
        (2, 26) to (2, 27) => {
          (2, 8) to (2, 9): (`X`)
        };
        (2, 40) to (2, 41) => {
          Global Y
        };
        (2, 42) to (2, 43) => {
          (2, 11) to (2, 12): (`Y`)
        };
        (3, 6) to (3, 7) => {
          (2, 11) to (2, 12): (`Y`)
        };
        (3, 11) to (3, 12) => {
          (2, 8) to (2, 9): (`X`)
        };
        (3, 16) to (3, 17) => {
          (2, 11) to (2, 12): (`Y`)
        };
        (4, 11) to (4, 12) => {
          (3, 4) to (3, 5): (`Z`)
        }] |}]

let%expect_test "declare_class_tparam" =
  print_ssa_test {|
declare class C<X, Y:X, Z = X> extends X<X> mixins Z<Z> implements Y<Y> {
  f<Z:Y>(X, Y): Z;
}
|};
    [%expect {|
      [
        (2, 21) to (2, 22) => {
          (2, 16) to (2, 17): (`X`)
        };
        (2, 28) to (2, 29) => {
          (2, 16) to (2, 17): (`X`)
        };
        (2, 39) to (2, 40) => {
          Global X
        };
        (2, 41) to (2, 42) => {
          (2, 16) to (2, 17): (`X`)
        };
        (2, 51) to (2, 52) => {
          Global Z
        };
        (2, 53) to (2, 54) => {
          (2, 24) to (2, 25): (`Z`)
        };
        (2, 67) to (2, 68) => {
          Global Y
        };
        (2, 69) to (2, 70) => {
          (2, 19) to (2, 20): (`Y`)
        };
        (3, 6) to (3, 7) => {
          (2, 19) to (2, 20): (`Y`)
        };
        (3, 9) to (3, 10) => {
          (2, 16) to (2, 17): (`X`)
        };
        (3, 12) to (3, 13) => {
          (2, 19) to (2, 20): (`Y`)
        };
        (3, 16) to (3, 17) => {
          (3, 4) to (3, 5): (`Z`)
        }] |}]

let%expect_test "class_expr_tparam" =
  print_ssa_test {|
var w = class <X, Y:X> extends X<X> implements Y<Y> {
  f<Z:Y>(x:X, y:Y) {
    let z: Z;
  }
}
|};
    [%expect {|
      [
        (2, 20) to (2, 21) => {
          (2, 15) to (2, 16): (`X`)
        };
        (2, 31) to (2, 32) => {
          Global X
        };
        (2, 33) to (2, 34) => {
          (2, 15) to (2, 16): (`X`)
        };
        (2, 47) to (2, 48) => {
          Global Y
        };
        (2, 49) to (2, 50) => {
          (2, 18) to (2, 19): (`Y`)
        };
        (3, 6) to (3, 7) => {
          (2, 18) to (2, 19): (`Y`)
        };
        (3, 11) to (3, 12) => {
          (2, 15) to (2, 16): (`X`)
        };
        (3, 16) to (3, 17) => {
          (2, 18) to (2, 19): (`Y`)
        };
        (4, 11) to (4, 12) => {
          (3, 4) to (3, 5): (`Z`)
        }] |}]

let%expect_test "import_type" =
  print_ssa_test {|
var x: S;
import { type S } from '';
|};
    [%expect {|
      [
        (2, 7) to (2, 8) => {
          (3, 14) to (3, 15): (`S`)
        }] |}]

let%expect_test "import_mix" =
  print_ssa_test {|
var x: S = t;
var a: W;
import { type S, t, w as y, typeof w as W } from '';
t;
y;
w;
|};
    [%expect {|
      [
        (2, 7) to (2, 8) => {
          (4, 14) to (4, 15): (`S`)
        };
        (2, 11) to (2, 12) => {
          (undeclared)
        };
        (3, 7) to (3, 8) => {
          (4, 40) to (4, 41): (`W`)
        };
        (5, 0) to (5, 1) => {
          (4, 17) to (4, 18): (`t`)
        };
        (6, 0) to (6, 1) => {
          (4, 25) to (4, 26): (`y`)
        };
        (7, 0) to (7, 1) => {
          Global w
        }] |}]

let%expect_test "import_def" =
  print_ssa_test {|
(NS: NST);
(ps: ns);
(ps: ms);
(1: a);
(2: b);
import type {a, b} from ''
import typeof ns from '';
import type ms from '';
import ps from ''
import * as NS from ''
import typeof * as NST from ''
(NS: NST);
(ps: ns);
(ps: ms);
(1: a);
(2: b);
|};
    [%expect {|
      [
        (2, 1) to (2, 3) => {
          (undeclared)
        };
        (2, 5) to (2, 8) => {
          (12, 19) to (12, 22): (`NST`)
        };
        (3, 1) to (3, 3) => {
          (undeclared)
        };
        (3, 5) to (3, 7) => {
          (8, 14) to (8, 16): (`ns`)
        };
        (4, 1) to (4, 3) => {
          (undeclared)
        };
        (4, 5) to (4, 7) => {
          (9, 12) to (9, 14): (`ms`)
        };
        (5, 4) to (5, 5) => {
          (7, 13) to (7, 14): (`a`)
        };
        (6, 4) to (6, 5) => {
          (7, 16) to (7, 17): (`b`)
        };
        (13, 1) to (13, 3) => {
          (11, 12) to (11, 14): (`NS`)
        };
        (13, 5) to (13, 8) => {
          (12, 19) to (12, 22): (`NST`)
        };
        (14, 1) to (14, 3) => {
          (10, 7) to (10, 9): (`ps`)
        };
        (14, 5) to (14, 7) => {
          (8, 14) to (8, 16): (`ns`)
        };
        (15, 1) to (15, 3) => {
          (10, 7) to (10, 9): (`ps`)
        };
        (15, 5) to (15, 7) => {
          (9, 12) to (9, 14): (`ms`)
        };
        (16, 4) to (16, 5) => {
          (7, 13) to (7, 14): (`a`)
        };
        (17, 4) to (17, 5) => {
          (7, 16) to (7, 17): (`b`)
        }] |}]

let%expect_test "inc" =
  print_ssa_test {|
let x = 0;
++x;
x++;
x;
  |};
  [%expect {|
    [
      (3, 2) to (3, 3) => {
        (2, 4) to (2, 5): (`x`)
      };
      (4, 0) to (4, 1) => {
        (3, 2) to (3, 3): (`x`)
      };
      (5, 0) to (5, 1) => {
        (4, 0) to (4, 1): (`x`)
      }] |}]

let%expect_test "inc_heap" =
  print_ssa_test {|
let x = { a: 0 };
++x.a;
x.a++;
x.a;
  |};
  [%expect {|
    [
      (3, 2) to (3, 3) => {
        (2, 4) to (2, 5): (`x`)
      };
      (4, 0) to (4, 1) => {
        (2, 4) to (2, 5): (`x`)
      };
      (4, 0) to (4, 3) => {
        number
      };
      (5, 0) to (5, 1) => {
        (2, 4) to (2, 5): (`x`)
      };
      (5, 0) to (5, 3) => {
        number
      }] |}]

let%expect_test "op_assign_heap" =
  print_ssa_test {|
let x = { a: 0 };
x.a += 42;
x.a;
  |};
  [%expect {|
    [
      (3, 0) to (3, 1) => {
        (2, 4) to (2, 5): (`x`)
      };
      (4, 0) to (4, 1) => {
        (2, 4) to (2, 5): (`x`)
      };
      (4, 0) to (4, 3) => {
        (3, 0) to (3, 3): (some property)
      }] |}]

let%expect_test "class1" =
  print_ssa_test {|
(C: void); // as a value, C is undefined and in the TDZ

declare var c: C; // as a type, C refers to the class below
c.foo;

class C {
    foo: number;
}
  |};
  [%expect {|
    [
      (2, 1) to (2, 2) => {
        (undeclared)
      };
      (4, 15) to (4, 16) => {
        (7, 6) to (7, 7): (`C`)
      };
      (5, 0) to (5, 1) => {
        (4, 12) to (4, 13): (`c`)
      }] |}]

let%expect_test "class2" =
  print_ssa_test {|
class C {
  foo: D;
}
class D extends C {
  bar;
}
  |};
  [%expect {|
    [
      (3, 7) to (3, 8) => {
        (5, 6) to (5, 7): (`D`)
      };
      (5, 16) to (5, 17) => {
        (2, 6) to (2, 7): (`C`)
      }] |}]

let%expect_test "class3" =
  print_ssa_test {|
C;
class C {
  x: C;
}
C;
  |};
  [%expect {|
    [
      (2, 0) to (2, 1) => {
        (undeclared)
      };
      (4, 5) to (4, 6) => {
        (3, 6) to (3, 7): (`C`)
      };
      (6, 0) to (6, 1) => {
        (3, 6) to (3, 7): (`C`)
      }] |}]

let%expect_test "class4" =
  print_ssa_test {|

function havoced() {
  C;
}

class C {
}
  |};
  [%expect {|
    [
      (4, 2) to (4, 3) => {
        (7, 6) to (7, 7): (`C`)
      }] |}]

let%expect_test "class5" =
  print_ssa_test {|

function havoc() {
  C = 42;
}

havoc();
C;

class C {
}
  |};
  [%expect {|
    [
      (7, 0) to (7, 5) => {
        (3, 9) to (3, 14): (`havoc`)
      };
      (8, 0) to (8, 1) => {
        (undeclared)
      }] |}]

let%expect_test "deps_recur_broken_init" =
  print_ssa_test {|
type T = number;
let x: T;
function f() {
  x = x;
}
  |};
  [%expect {|
    [
      (3, 7) to (3, 8) => {
        (2, 5) to (2, 6): (`T`)
      };
      (5, 6) to (5, 7) => {
        (3, 4) to (3, 5): (`x`)
      }] |}]

let%expect_test "class3" =
  print_ssa_test {|
class C {
  foo: D;
}
class D extends C {
  bar;
}
  |};
  [%expect {|
    [
      (3, 7) to (3, 8) => {
        (5, 6) to (5, 7): (`D`)
      };
      (5, 16) to (5, 17) => {
        (2, 6) to (2, 7): (`C`)
      }] |}]

let%expect_test "enum" =
  print_ssa_test {|
function havoced() {
  var x: E = E.Foo
}
enum E {
  Foo
}
  |};
  [%expect {|
    [
      (3, 9) to (3, 10) => {
        (5, 5) to (5, 6): (`E`)
      };
      (3, 13) to (3, 14) => {
        (5, 5) to (5, 6): (`E`)
      }] |}]

let%expect_test "react_jsx" =
  print_ssa_test {|
const React = require('react');

<div />;
  |};
    [%expect {|
      [
        (2, 14) to (2, 21) => {
          Global require
        };
        (4, 0) to (4, 7) => {
          (2, 6) to (2, 11): (`React`)
        };
        (4, 1) to (4, 4) => {
          Global div
        }] |}]

let%expect_test "unreachable_jsx" =
  print_ssa_test {|
const React = require('react');
throw new Error();
<Component />;
|};
    [%expect {|
      [
        (2, 14) to (2, 21) => {
          Global require
        };
        (3, 10) to (3, 15) => {
          Global Error
        };
        (4, 0) to (4, 13) => {
          unreachable
        };
        (4, 1) to (4, 10) => {
          unreachable
        }] |}]

let%expect_test "custom_jsx_pragma" =
  print_ssa_test ~custom_jsx:(Some "createMikesCoolElement") {|
  let createMikesCoolElement = (null: any);
<FirstElement />
function time_to_create_some_elements_bro() {
  <SecondElement />
}
|};
    [%expect {|
      [
        (3, 0) to (3, 16) => {
          (2, 6) to (2, 28): (`createMikesCoolElement`)
        };
        (3, 1) to (3, 13) => {
          Global FirstElement
        };
        (5, 2) to (5, 19) => {
          (2, 6) to (2, 28): (`createMikesCoolElement`)
        };
        (5, 3) to (5, 16) => {
          Global SecondElement
        }] |}]

let%expect_test "jsx_pragma_member_expr" =
  print_ssa_test ~custom_jsx:(Some "Test.f") {|
function Component() {}
<Component />
|};
    [%expect {|
      [
        (1, 0) to (1, 4) => {
          Global Test
        };
        (3, 1) to (3, 10) => {
          (2, 9) to (2, 18): (`Component`)
        }]
       |}]

let%expect_test "automatic_react_runtime" =
  print_ssa_test ~react_runtime_automatic:true {|
  let createMikesCoolElement = (null: any);
<FirstElement />
function time_to_create_some_elements_bro() {
  <SecondElement />
}
|};
    [%expect {|
      [
        (3, 1) to (3, 13) => {
          Global FirstElement
        };
        (5, 3) to (5, 16) => {
          Global SecondElement
        }] |}]

let%expect_test "react_jsx" =
  print_ssa_test {|
const React = require('react');

<div />;
|};
    [%expect {|
      [
        (2, 14) to (2, 21) => {
          Global require
        };
        (4, 0) to (4, 7) => {
          (2, 6) to (2, 11): (`React`)
        };
        (4, 1) to (4, 4) => {
          Global div
        }] |}]

let%expect_test "unreachable_jsx" =
  print_ssa_test {|
const React = require('react');
throw new Error();
<Component />;
|};
    [%expect {|
      [
        (2, 14) to (2, 21) => {
          Global require
        };
        (3, 10) to (3, 15) => {
          Global Error
        };
        (4, 0) to (4, 13) => {
          unreachable
        };
        (4, 1) to (4, 10) => {
          unreachable
        }] |}]

let%expect_test "switch_reread_discriminant" =
  print_ssa_test {|
let y = {};
switch (y.x) { // Does not report a Projection
    case 'ONE': break;
    case 'TWO': break;
    default:
      (y.x: empty);
}
|};
    [%expect {|
      [
        (3, 8) to (3, 9) => {
          (2, 4) to (2, 5): (`y`)
        };
        (4, 4) to (4, 22) => {
          (2, 4) to (2, 5): (`y`)
        };
        (5, 4) to (5, 22) => {
          {refinement = Not (SentinelR x); writes = (2, 4) to (2, 5): (`y`)}
        };
        (7, 7) to (7, 8) => {
          {refinement = Not (SentinelR x); writes = {refinement = Not (SentinelR x); writes = (2, 4) to (2, 5): (`y`)}}
        };
        (7, 7) to (7, 10) => {
          {refinement = Not (TWO); writes = {refinement = Not (ONE); writes = projection at (3, 8) to (3, 11)}}
        }] |}]

let%expect_test "no_refinement_write_on_indexed" =
  print_ssa_test {|
let x = {};
let y = 'str';
x[y] = 3;
x[y]; // Should not report an entry
|};
    [%expect {|
      [
        (4, 0) to (4, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 2) to (4, 3) => {
          (3, 4) to (3, 5): (`y`)
        };
        (5, 0) to (5, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 2) to (5, 3) => {
          (3, 4) to (3, 5): (`y`)
        }] |}]

let%expect_test "switch_no_default" =
  print_ssa_test {|
let y = 'ONE';
switch (y) {
    case 'ONE': break;
    case 'TWO': break;
}
|};
    [%expect {|
      [
        (3, 0) to (6, 1) => {
          {refinement = Not (TWO); writes = {refinement = Not (ONE); writes = (2, 4) to (2, 5): (`y`)}}
        };
        (3, 8) to (3, 9) => {
          (2, 4) to (2, 5): (`y`)
        }] |}]

let%expect_test "switch_exhaustive_return" =
  print_ssa_test {|
let obj = {};
  switch (obj.k) {
    case 'a':
      throw 0;
    case 'b':
      throw 1;
  }
|};
    [%expect {|
      [
        (3, 2) to (8, 3) => {
          {refinement = Not (b); writes = {refinement = Not (a); writes = projection at (3, 10) to (3, 15)}}
        };
        (3, 10) to (3, 13) => {
          (2, 4) to (2, 7): (`obj`)
        };
        (4, 4) to (5, 14) => {
          (2, 4) to (2, 7): (`obj`)
        };
        (6, 4) to (7, 14) => {
          {refinement = Not (SentinelR k); writes = (2, 4) to (2, 7): (`obj`)}
        }] |}]

let%expect_test "switch_exhaustive_fallthrough_return" =
  print_ssa_test {|
let obj = {};
  switch (obj.k) {
    case 'a':
    case 'b':
      throw 1;
  }
|};
    [%expect {|
      [
        (3, 2) to (7, 3) => {
          {refinement = Not (b); writes = {refinement = Not (a); writes = projection at (3, 10) to (3, 15)}}
        };
        (3, 10) to (3, 13) => {
          (2, 4) to (2, 7): (`obj`)
        };
        (4, 4) to (4, 13) => {
          (2, 4) to (2, 7): (`obj`)
        };
        (5, 4) to (6, 14) => {
          {refinement = Not (SentinelR k); writes = (2, 4) to (2, 7): (`obj`)}
        }] |}]

let%expect_test "reference_before_declaration" =
  print_ssa_test {|
  _const;
  _let;
  _var;
  _func1;
  _func2;
  _func3;
  _func4;
  _class;
  E;

  const _const = 3;
  let _let = 3;
  var _var = 3;
  function _func1() {}
  export function _func2() {}
  export default function _func3() {}
  declare export function _func4(): void
class _class {}
  enum E { A }
|};
    [%expect {|
      [
        (2, 2) to (2, 8) => {
          (undeclared)
        };
        (3, 2) to (3, 6) => {
          (undeclared)
        };
        (4, 2) to (4, 6) => {
          (uninitialized)
        };
        (5, 2) to (5, 8) => {
          (15, 11) to (15, 17): (`_func1`)
        };
        (6, 2) to (6, 8) => {
          (16, 18) to (16, 24): (`_func2`)
        };
        (7, 2) to (7, 8) => {
          (17, 26) to (17, 32): (`_func3`)
        };
        (8, 2) to (8, 8) => {
          declared function (18, 26) to (18, 32)
        };
        (9, 2) to (9, 8) => {
          (undeclared)
        };
        (10, 2) to (10, 3) => {
          (undeclared)
        }] |}]

let%expect_test "declaration_declares_undeclared" =
  print_ssa_test {|
x;
x = 3; // Does not count as a write until LHS is declared
x;
let x;
x;
x = 3;
x;
|};
    [%expect {|
      [
        (2, 0) to (2, 1) => {
          (undeclared)
        };
        (4, 0) to (4, 1) => {
          (undeclared)
        };
        (6, 0) to (6, 1) => {
          (uninitialized)
        };
        (8, 0) to (8, 1) => {
          (7, 0) to (7, 1): (`x`)
        }] |}]

let%expect_test "undeclared_havoc_no_writes" =
  print_ssa_test {|
console.log('foo');
x;
let x;
|};
    [%expect {|
      [
        (2, 0) to (2, 7) => {
          Global console
        };
        (3, 0) to (3, 1) => {
          (undeclared)
        }] |}]

let%expect_test "undeclared_havoc_function_writes" =
  print_ssa_test {|
function f() { x = 3 }
console.log('foo');
x;
let x;
|};
    [%expect {|
      [
        (3, 0) to (3, 7) => {
          Global console
        };
        (4, 0) to (4, 1) => {
          (undeclared)
        }] |}]

let%expect_test "undeclared_enter_function_scope" =
  print_ssa_test {|
function f() { x; }
let x;
x = 3;
|};
    [%expect {|
      [
        (2, 15) to (2, 16) => {
          (4, 0) to (4, 1): (`x`)
        }] |}]

let%expect_test "default_switch_refinement" =
  print_ssa_test {|
type Enum = 'ONE' | 'TWO';
type Selection = { x: 'ONE' } | { x: 'TWO' } | { x: 'NONE' }

type Rule = {
  x: Enum,
  y: Selection,
}

function foo(r: Rule) {
  const x = r.x;
  const y = r.y;
  if (y.x === x) {
    switch (y.x) {
      case 'ONE': break;
      case 'TWO': break;
      default: (y.x: empty);
    }
  }
}
|};
    [%expect {|
      [
        (6, 5) to (6, 9) => {
          (2, 5) to (2, 9): (`Enum`)
        };
        (7, 5) to (7, 14) => {
          (3, 5) to (3, 14): (`Selection`)
        };
        (10, 16) to (10, 20) => {
          (5, 5) to (5, 9): (`Rule`)
        };
        (11, 12) to (11, 13) => {
          (10, 13) to (10, 14): (`r`)
        };
        (12, 12) to (12, 13) => {
          (10, 13) to (10, 14): (`r`)
        };
        (13, 6) to (13, 7) => {
          (12, 8) to (12, 9): (`y`)
        };
        (13, 14) to (13, 15) => {
          (11, 8) to (11, 9): (`x`)
        };
        (14, 12) to (14, 13) => {
          {refinement = SentinelR x; writes = (12, 8) to (12, 9): (`y`)}
        };
        (14, 12) to (14, 15) => {
          {refinement = EqR; writes = projection at (13, 6) to (13, 9)}
        };
        (15, 6) to (15, 24) => {
          {refinement = SentinelR x; writes = (12, 8) to (12, 9): (`y`)}
        };
        (16, 6) to (16, 24) => {
          {refinement = Not (SentinelR x); writes = {refinement = SentinelR x; writes = (12, 8) to (12, 9): (`y`)}}
        };
        (17, 16) to (17, 17) => {
          {refinement = Not (SentinelR x); writes = {refinement = Not (SentinelR x); writes = {refinement = SentinelR x; writes = (12, 8) to (12, 9): (`y`)}}}
        };
        (17, 16) to (17, 19) => {
          {refinement = Not (TWO); writes = {refinement = Not (ONE); writes = {refinement = EqR; writes = projection at (13, 6) to (13, 9)}}}
        }] |}]

let%expect_test "prop_exists" =
  print_ssa_test {|
const x = {foo: 3};

if (x.foo) {
  x;
  x.foo;
}
|};
    [%expect {|
[
  (4, 4) to (4, 5) => {
    (2, 6) to (2, 7): (`x`)
  };
  (5, 2) to (5, 3) => {
    {refinement = PropTruthyR (foo); writes = (2, 6) to (2, 7): (`x`)}
  };
  (6, 2) to (6, 3) => {
    {refinement = PropTruthyR (foo); writes = (2, 6) to (2, 7): (`x`)}
  };
  (6, 2) to (6, 7) => {
    {refinement = Truthy; writes = projection at (4, 4) to (4, 9)}
  }] |}]

let%expect_test "try_catch_env_merging" =
  print_ssa_test {|
function maz2() {
  let x: number[] = [];
  try {
    throw new Error(`just capturing a stack trace`);
  } catch (e) {
    var a: string = x[0];
  }
  var c: string = x[0]; // reachable
}
|};
    [%expect {|
      [
        (5, 14) to (5, 19) => {
          Global Error
        };
        (7, 20) to (7, 21) => {
          (3, 6) to (3, 7): (`x`)
        };
        (9, 18) to (9, 19) => {
          (3, 6) to (3, 7): (`x`)
        }] |}]

let%expect_test "try_catch_catch_throws" =
  print_ssa_test {|
function bar(response) {
    var payload;
    try {
        payload = JSON.parse(response);
    } catch (e) {
        throw new Error('...');
    }
    // here via [try] only.
    if (payload.error) {    // ok
        // ...
    }
}
|};
    [%expect {|
      [
        (5, 18) to (5, 22) => {
          Global JSON
        };
        (5, 29) to (5, 37) => {
          (2, 13) to (2, 21): (`response`)
        };
        (7, 18) to (7, 23) => {
          Global Error
        };
        (10, 8) to (10, 15) => {
          (5, 8) to (5, 15): (`payload`)
        }] |}]

let%expect_test "try_catch_throw_in_both_then_finally" =
  print_ssa_test {|
function bar(response) {
    var payload;
    try {
      throw new Error();
    } catch (e) {
      throw new Error();
    } finally {
      payload = 3;
    }
    payload; // Dead
}
|};
    [%expect {|
      [
        (5, 16) to (5, 21) => {
          Global Error
        };
        (7, 16) to (7, 21) => {
          Global Error
        };
        (11, 4) to (11, 11) => {
          unreachable
        }] |}]

let%expect_test "try_throw_catch_finally" =
  print_ssa_test {|
function bar(response) {
    var payload;
    try {
      throw new Error();
    } catch (e) {
      payload = 4;
    } finally {
      payload;
    }
    payload;
}
|};
    [%expect {|
      [
        (5, 16) to (5, 21) => {
          Global Error
        };
        (9, 6) to (9, 13) => {
          (uninitialized),
          (7, 6) to (7, 13): (`payload`)
        };
        (11, 4) to (11, 11) => {
          (7, 6) to (7, 13): (`payload`)
        }] |}]

let%expect_test "try_catch_throw_finally" =
  print_ssa_test {|
function bar(response) {
    let payload;
    try {
      payload = 3;
    } catch (e) {
      payload = 4;
      throw new Error();
    } finally {
      payload;
    }
    payload; // = 3
}
|};
    [%expect {|
      [
        (8, 16) to (8, 21) => {
          Global Error
        };
        (10, 6) to (10, 13) => {
          (uninitialized),
          (5, 6) to (5, 13): (`payload`),
          (7, 6) to (7, 13): (`payload`)
        };
        (12, 4) to (12, 11) => {
          (5, 6) to (5, 13): (`payload`)
        }] |}]

let%expect_test "try_catch_finally_throw" =
  print_ssa_test {|
function bar(response) {
    let payload;
    try {
      payload = 3;
    } catch (e) {
      payload = 4;
    } finally {
      payload = 5;
      throw new Error();
    }
    payload; // Dead
}
|};
    [%expect {|
      [
        (10, 16) to (10, 21) => {
          Global Error
        };
        (12, 4) to (12, 11) => {
          unreachable
        }] |}]

let%expect_test "try_throw_catch_throw_finally_throw" =
  print_ssa_test {|
function bar(response) {
    let payload;
    try {
      payload = 3;
      throw new Error()
    } catch (e) {
      payload = 4;
      throw new Error()
    } finally {
      payload = 5;
      throw new Error();
    }
    payload; // Dead
}
|};
    [%expect {|
      [
        (6, 16) to (6, 21) => {
          Global Error
        };
        (9, 16) to (9, 21) => {
          Global Error
        };
        (12, 16) to (12, 21) => {
          Global Error
        };
        (14, 4) to (14, 11) => {
          unreachable
        }] |}]

let%expect_test "try_throw_catch_throw_finally" =
  print_ssa_test {|
function bar(response) {
    let payload;
    try {
      try {
        payload = 3;
        throw new Error()
      } catch (e) {
        payload = 4;
        throw new Error()
      } finally {
        payload = 5;
      }
      payload; // Dead
    } catch (e) {
      payload; // = 5 | uninitialized
    }
}
|};
    [%expect {|
      [
        (7, 18) to (7, 23) => {
          Global Error
        };
        (10, 18) to (10, 23) => {
          Global Error
        };
        (14, 6) to (14, 13) => {
          unreachable
        };
        (16, 6) to (16, 13) => {
          (uninitialized),
          (12, 8) to (12, 15): (`payload`)
        }] |}]

let%expect_test "exports_special" =
  print_ssa_test {|
exports.foo = 1;
|};
    [%expect {|
      [
        (2, 0) to (2, 7) => {
          Global exports
        }] |}]

let%expect_test "module_dot_export_special" =
  print_ssa_test {|
module.exports;
|};
    [%expect {|
      [
        (2, 0) to (2, 6) => {
          Global module
        };
        (2, 0) to (2, 14) => {
          Global exports
        }] |}]

let%expect_test "exports_as_global" =
  print_ssa_test ~lib:true {|
exports.foo = 1;
|};
    [%expect {|
      [
        (2, 0) to (2, 7) => {
          Global exports
        }] |}]

let%expect_test "module_dot_export_as_global" =
  print_ssa_test ~lib:true {|
module.exports;
|};
    [%expect {|
      [
        (2, 0) to (2, 6) => {
          Global module
        }] |}]

let%expect_test "import_havoc" =
  print_ssa_test {|
import {func} from './a';

function f() {
  func();
}
|};
    [%expect {|
      [
        (5, 2) to (5, 6) => {
          (2, 8) to (2, 12): (`func`)
        }] |}]

let%expect_test "test27" =
  print_ssa_test {|
if (!x.a) { x.c; } else { x.b; }
|};
    [%expect {|
      [
        (2, 5) to (2, 6) => {
          Global x
        };
        (2, 12) to (2, 13) => {
          {refinement = Not (PropTruthyR (a)); writes = Global x}
        };
        (2, 26) to (2, 27) => {
          {refinement = PropTruthyR (a); writes = Global x}
        }] |}]

let%expect_test "conjunct" =
  print_ssa_test {|
if (x.a && x.b)
  { x.a; x.b }
else
  { x.a; x.b }
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (2, 11) to (2, 12) => {
          {refinement = PropTruthyR (a); writes = Global x}
        };
        (3, 4) to (3, 5) => {
          {refinement = And (PropTruthyR (a), PropTruthyR (b)); writes = Global x}
        };
        (3, 4) to (3, 7) => {
          {refinement = Truthy; writes = projection at (2, 4) to (2, 7)}
        };
        (3, 9) to (3, 10) => {
          {refinement = And (PropTruthyR (a), PropTruthyR (b)); writes = Global x}
        };
        (3, 9) to (3, 12) => {
          {refinement = Truthy; writes = projection at (2, 11) to (2, 14)}
        };
        (5, 4) to (5, 5) => {
          {refinement = Or (Not (PropTruthyR (a)), Not (PropTruthyR (b))); writes = Global x}
        };
        (5, 9) to (5, 10) => {
          {refinement = Or (Not (PropTruthyR (a)), Not (PropTruthyR (b))); writes = Global x}
        }] |}]

let%expect_test "disjunct" =
  print_ssa_test {|
if (x.a || x.b)
  { x.a; x.b }
else
  { x.a; x.b }
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (2, 11) to (2, 12) => {
          {refinement = Not (PropTruthyR (a)); writes = Global x}
        };
        (3, 4) to (3, 5) => {
          {refinement = Or (PropTruthyR (a), PropTruthyR (b)); writes = Global x}
        };
        (3, 9) to (3, 10) => {
          {refinement = Or (PropTruthyR (a), PropTruthyR (b)); writes = Global x}
        };
        (5, 4) to (5, 5) => {
          {refinement = And (Not (PropTruthyR (a)), Not (PropTruthyR (b))); writes = Global x}
        };
        (5, 4) to (5, 7) => {
          {refinement = Not (Truthy); writes = projection at (2, 4) to (2, 7)}
        };
        (5, 9) to (5, 10) => {
          {refinement = And (Not (PropTruthyR (a)), Not (PropTruthyR (b))); writes = Global x}
        };
        (5, 9) to (5, 12) => {
          {refinement = Not (Truthy); writes = projection at (2, 11) to (2, 14)}
        }] |}]

let%expect_test "complex" =
  print_ssa_test {|
if ((x.a || x.b) && x.c)
  { x.a; x.b; x.c }
else
  { x.a; x.b; x.c }
|};
    [%expect {|
      [
        (2, 5) to (2, 6) => {
          Global x
        };
        (2, 12) to (2, 13) => {
          {refinement = Not (PropTruthyR (a)); writes = Global x}
        };
        (2, 20) to (2, 21) => {
          {refinement = Or (PropTruthyR (a), PropTruthyR (b)); writes = Global x}
        };
        (3, 4) to (3, 5) => {
          {refinement = And (Or (PropTruthyR (a), PropTruthyR (b)), PropTruthyR (c)); writes = Global x}
        };
        (3, 9) to (3, 10) => {
          {refinement = And (Or (PropTruthyR (a), PropTruthyR (b)), PropTruthyR (c)); writes = Global x}
        };
        (3, 14) to (3, 15) => {
          {refinement = And (Or (PropTruthyR (a), PropTruthyR (b)), PropTruthyR (c)); writes = Global x}
        };
        (3, 14) to (3, 17) => {
          {refinement = Truthy; writes = projection at (2, 20) to (2, 23)}
        };
        (5, 4) to (5, 5) => {
          {refinement = Or (And (Not (PropTruthyR (a)), Not (PropTruthyR (b))), Not (PropTruthyR (c))); writes = Global x}
        };
        (5, 9) to (5, 10) => {
          {refinement = Or (And (Not (PropTruthyR (a)), Not (PropTruthyR (b))), Not (PropTruthyR (c))); writes = Global x}
        };
        (5, 14) to (5, 15) => {
          {refinement = Or (And (Not (PropTruthyR (a)), Not (PropTruthyR (b))), Not (PropTruthyR (c))); writes = Global x}
        }] |}]

let%expect_test "changeset" =
  print_ssa_test {|
if (x && x.a)
  { x.a; }
else
  { x.a; }
x.a;
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (2, 9) to (2, 10) => {
          {refinement = Truthy; writes = Global x}
        };
        (3, 4) to (3, 5) => {
          {refinement = And (Truthy, PropTruthyR (a)); writes = Global x}
        };
        (3, 4) to (3, 7) => {
          {refinement = Truthy; writes = projection at (2, 9) to (2, 12)}
        };
        (5, 4) to (5, 5) => {
          {refinement = Or (Not (Truthy), Not (PropTruthyR (a))); writes = Global x}
        };
        (6, 0) to (6, 1) => {
          Global x
        }] |}]

let%expect_test "no_changeset" =
  print_ssa_test {|
if (x.a)
  { x.a; }
else
  { x.a; }
x.a;
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (3, 4) to (3, 5) => {
          {refinement = PropTruthyR (a); writes = Global x}
        };
        (3, 4) to (3, 7) => {
          {refinement = Truthy; writes = projection at (2, 4) to (2, 7)}
        };
        (5, 4) to (5, 5) => {
          {refinement = Not (PropTruthyR (a)); writes = Global x}
        };
        (5, 4) to (5, 7) => {
          {refinement = Not (Truthy); writes = projection at (2, 4) to (2, 7)}
        };
        (6, 0) to (6, 1) => {
          Global x
        }] |}]

let%expect_test "changeset_update" =
  print_ssa_test {|
if (x.a)
  { x.a = 42 }
x.a;
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (3, 4) to (3, 5) => {
          {refinement = PropTruthyR (a); writes = Global x}
        };
        (4, 0) to (4, 1) => {
          Global x
        };
        (4, 0) to (4, 3) => {
          (3, 4) to (3, 7): (some property),
          {refinement = Not (Truthy); writes = projection at (2, 4) to (2, 7)}
        }] |}]

let%expect_test "changeset_pre_exist" =
  print_ssa_test {|
if(x && x.a) {
  if(x && x.a) {}
  else {
    x.a;
  }
  x.a;
}
|};
    [%expect {|
      [
        (2, 3) to (2, 4) => {
          Global x
        };
        (2, 8) to (2, 9) => {
          {refinement = Truthy; writes = Global x}
        };
        (3, 5) to (3, 6) => {
          {refinement = And (Truthy, PropTruthyR (a)); writes = Global x}
        };
        (3, 10) to (3, 11) => {
          {refinement = Truthy; writes = {refinement = And (Truthy, PropTruthyR (a)); writes = Global x}}
        };
        (3, 10) to (3, 13) => {
          {refinement = Truthy; writes = projection at (2, 8) to (2, 11)}
        };
        (5, 4) to (5, 5) => {
          {refinement = Or (Not (Truthy), Not (PropTruthyR (a))); writes = {refinement = And (Truthy, PropTruthyR (a)); writes = Global x}}
        };
        (5, 4) to (5, 7) => {
          {refinement = Truthy; writes = projection at (2, 8) to (2, 11)}
        };
        (7, 2) to (7, 3) => {
          {refinement = And (Truthy, PropTruthyR (a)); writes = Global x}
        };
        (7, 2) to (7, 5) => {
          {refinement = Truthy; writes = projection at (2, 8) to (2, 11)}
        }] |}]

let%expect_test "optional_refi" =
  print_ssa_test {|
var x: ?Array<number> = null;
x?.[x[0]];
if(x?.[x[0]]) { x; }
|};
    [%expect {|
      [
        (2, 8) to (2, 13) => {
          Global Array
        };
        (3, 0) to (3, 1) => {
          (2, 4) to (2, 5): (`x`)
        };
        (3, 4) to (3, 5) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 3) to (4, 4) => {
          (2, 4) to (2, 5): (`x`)
        };
        (4, 7) to (4, 8) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`x`)}
        };
        (4, 16) to (4, 17) => {
          {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`x`)}
        }] |}]

let%expect_test "optional_refi2" =
  print_ssa_test {|
declare var x: ?{y: {f: (mixed) => void, z?: {w: mixed => void}}};
x?.y.f(x);
x?.y.z?.w(x.y.z);
(x?.y).f(x);
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (2, 12) to (2, 13): (`x`)
        };
        (3, 7) to (3, 8) => {
          {refinement = And (Not (Maybe), Not (PropNullishR y)); writes = (2, 12) to (2, 13): (`x`)}
        };
        (4, 0) to (4, 1) => {
          (2, 12) to (2, 13): (`x`)
        };
        (4, 10) to (4, 11) => {
          {refinement = And (Not (Maybe), Not (PropNullishR y)); writes = (2, 12) to (2, 13): (`x`)}
        };
        (4, 10) to (4, 13) => {
          {refinement = Not (PropNullishR z); writes = projection at (4, 0) to (4, 4)}
        };
        (4, 10) to (4, 15) => {
          {refinement = And (Not (Maybe), Not (PropNullishR w)); writes = projection at (4, 0) to (4, 6)}
        };
        (5, 1) to (5, 2) => {
          (2, 12) to (2, 13): (`x`)
        };
        (5, 9) to (5, 10) => {
          (2, 12) to (2, 13): (`x`)
        };
        (4, 0) to (4, 4) => invalidated refinement by {
          function call at (3, 0) to (3, 9)
        };
        (5, 1) to (5, 5) => invalidated refinement by {
          function call at (4, 0) to (4, 16)
        }] |}]

let%expect_test "optional_refi3" =
  print_ssa_test {|
declare var x: mixed;
if (x?.a === 42) {
  x;
  x.a;
} else {
  x;
  x.a;
}
x;
x.a;
|};
    [%expect {|
      [
        (3, 4) to (3, 5) => {
          (2, 12) to (2, 13): (`x`)
        };
        (4, 2) to (4, 3) => {
          {refinement = And (And (Not (Maybe), Not (PropNullishR a)), SentinelR a); writes = (2, 12) to (2, 13): (`x`)}
        };
        (5, 2) to (5, 3) => {
          {refinement = And (And (Not (Maybe), Not (PropNullishR a)), SentinelR a); writes = (2, 12) to (2, 13): (`x`)}
        };
        (5, 2) to (5, 5) => {
          {refinement = 42; writes = projection at (3, 4) to (3, 8)}
        };
        (7, 2) to (7, 3) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (Not (PropNullishR a))), Not (SentinelR a)); writes = (2, 12) to (2, 13): (`x`)}
        };
        (8, 2) to (8, 3) => {
          {refinement = Or (Or (Not (Not (Maybe)), Not (Not (PropNullishR a))), Not (SentinelR a)); writes = (2, 12) to (2, 13): (`x`)}
        };
        (10, 0) to (10, 1) => {
          (2, 12) to (2, 13): (`x`)
        };
        (11, 0) to (11, 1) => {
          (2, 12) to (2, 13): (`x`)
        }] |}]

let%expect_test "dead_code_inc" =
  print_ssa_test {|
function f() {
    return;
    x += y;
}
|};
    [%expect {|
      [
        (4, 4) to (4, 5) => {
          unreachable
        };
        (4, 9) to (4, 10) => {
          unreachable
        }] |}]

let%expect_test "instanceof_mem" =
  print_ssa_test {|
if (x instanceof A.B) {
  x;
}
|};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (2, 17) to (2, 18) => {
          Global A
        };
        (3, 2) to (3, 3) => {
          {refinement = instanceof; writes = Global x}
        }] |}]

let%expect_test "instanceof_mem" =
  print_ssa_test {|
function test() {
  try {
    return;
  } catch {}
  for (let i = 0; ; i++) { }
}

|};
    [%expect {|
      [
        (6, 20) to (6, 21) => {
          (6, 11) to (6, 12): (`i`)
        }] |}]

let%expect_test "gen_next" =
  print_ssa_test {|
function *f() {
  yield;
}
|};
    [%expect {|
      [
        (3, 2) to (3, 7) => {
          (2, 13) to (2, 13): (next)
        }] |}]

let%expect_test "emp_array" =
  print_ssa_test {|
var x = [];
x;
function f() {
  x;
}
|};
    [%expect {|
      [
        (3, 0) to (3, 1) => {
          (empty array) (2, 4) to (2, 5): (`x`)
        };
        (5, 2) to (5, 3) => {
          (empty array) (2, 4) to (2, 5): (`x`)
        }] |}]

let%expect_test "emp_array2" =
  print_ssa_test {|
var x = [];
x = [10];
x;
function f() {
  x;
}
|};
    [%expect {|
      [
        (4, 0) to (4, 1) => {
          (3, 0) to (3, 1): (`x`)
        };
        (6, 2) to (6, 3) => {
          (3, 0) to (3, 1): (`x`),
          (empty array) (2, 4) to (2, 5): (`x`)
        }] |}]

let%expect_test "declare_module" =
  print_ssa_test {|
declare module F { }
|};
    [%expect {|
      [
        ] |}]

let%expect_test "declare_namespace" =
  print_ssa_test {|
const c = F.v;
declare namespace F {
  declare type T = string;
  declare const v: string;
}
const c = F.v;
type T = F.T;
|};
    [%expect {|
      [
        (2, 10) to (2, 11) => {
          (3, 18) to (3, 19): (`F`)
        };
        (7, 10) to (7, 11) => {
          (3, 18) to (3, 19): (`F`)
        };
        (8, 9) to (8, 10) => {
          (3, 18) to (3, 19): (`F`)
        }] |}]

let%expect_test "declare_type_only_namespace" =
  print_ssa_test {|
declare namespace F1 {
  declare type T = string;
}
declare namespace F2 {
  type T = string;
}
type T1 = F1.T;
type T2 = F2.T;
|};
    [%expect {|
      [
        (8, 10) to (8, 12) => {
          (2, 18) to (2, 20): (`F1`)
        };
        (9, 10) to (9, 12) => {
          (5, 18) to (5, 20): (`F2`)
        }] |}]

let%expect_test "delete_member" =
  print_ssa_test {|
delete foo.bar;
foo.bar;
invalidation();
foo.bar;
foo.bar = 3;
foo.bar;
|};
    [%expect {|
      [
        (2, 7) to (2, 10) => {
          Global foo
        };
        (3, 0) to (3, 3) => {
          Global foo
        };
        (3, 0) to (3, 7) => {
          undefined
        };
        (4, 0) to (4, 12) => {
          Global invalidation
        };
        (5, 0) to (5, 3) => {
          Global foo
        };
        (6, 0) to (6, 3) => {
          Global foo
        };
        (7, 0) to (7, 3) => {
          Global foo
        };
        (7, 0) to (7, 7) => {
          (6, 0) to (6, 7): (some property)
        }] |}]

let%expect_test "exclude_syms" =
  let exclude_syms = SSet.empty |> SSet.add "foo" |> SSet.add "Bar" in
  (* Ensure that defs and writes on excluded names have no effect. *)
  print_ssa_test ~exclude_syms {|
let foo1 = 0
foo1

let foo = 0
foo
foo = 1
foo
foo++
foo
delete foo
foo
type Bar = string;
declare var b1: Bar;
declare var b2: {bar: Bar};
|};
    [%expect {|
      [
        (3, 0) to (3, 4) => {
          (2, 4) to (2, 8): (`foo1`)
        };
        (6, 0) to (6, 3) => {
          Global foo
        };
        (8, 0) to (8, 3) => {
          Global foo
        };
        (9, 0) to (9, 3) => {
          Global foo
        };
        (10, 0) to (10, 3) => {
          Global foo
        };
        (11, 7) to (11, 10) => {
          Global foo
        };
        (12, 0) to (12, 3) => {
          Global foo
        };
        (14, 16) to (14, 19) => {
          Global Bar
        };
        (15, 22) to (15, 25) => {
          Global Bar
        }] |}]

let%expect_test "annot_this" =
  print_ssa_test {|
function f(this: number) {
  this;
}
|};
    [%expect {|
      [
        (3, 2) to (3, 6) => {
          (2, 11) to (2, 23): (this)
        }] |}]

let%expect_test "decl_enum" =
  print_ssa_test {|
type Props = X
var y = X;
export enum X {
  AGE,
}
|};
    [%expect {|
      [
        (2, 13) to (2, 14) => {
          (undeclared)
        };
        (3, 8) to (3, 9) => {
          (undeclared)
        }] |}]

let%expect_test "logic_op_assign_repeat" =
  print_ssa_test {|
function member_op_assignment_refinement_ok(o: {p: ?number}) {
  o.p &&= 3;
  o.p &&= 3;
  o.p &&= 3;
}
|};
    [%expect {|
      [
        (3, 2) to (3, 3) => {
          (2, 44) to (2, 45): (`o`)
        };
        (4, 2) to (4, 3) => {
          (2, 44) to (2, 45): (`o`)
        };
        (4, 2) to (4, 5) => {
          (3, 2) to (3, 5): (some property)
        };
        (5, 2) to (5, 3) => {
          (2, 44) to (2, 45): (`o`)
        };
        (5, 2) to (5, 5) => {
          (4, 2) to (4, 5): (some property)
        }]
        |}]

let%expect_test "logic_op_assign_repeat_existing_refi" =
  print_ssa_test {|
function member_op_assignment_refinement_ok(o: {p: ?number}) {
  if (o.p === 0) {
    o.p ??= 3;
    o.p &&= (o.p && 3);
    o.p ||= (o.p && 3);
  }
}
|};
    [%expect {|
      [
        (3, 6) to (3, 7) => {
          (2, 44) to (2, 45): (`o`)
        };
        (4, 4) to (4, 5) => {
          {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}
        };
        (4, 4) to (4, 7) => {
          {refinement = 0; writes = projection at (3, 6) to (3, 9)}
        };
        (5, 4) to (5, 5) => {
          {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}
        };
        (5, 4) to (5, 7) => {
          (4, 4) to (4, 7): (some property)
        };
        (5, 13) to (5, 14) => {
          {refinement = PropTruthyR (p); writes = {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}}
        };
        (5, 13) to (5, 16) => {
          {refinement = Truthy; writes = (4, 4) to (4, 7): (some property)}
        };
        (6, 4) to (6, 5) => {
          {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}
        };
        (6, 4) to (6, 7) => {
          (5, 4) to (5, 7): (some property)
        };
        (6, 13) to (6, 14) => {
          {refinement = Not (PropTruthyR (p)); writes = {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}}
        };
        (6, 13) to (6, 16) => {
          {refinement = Not (Truthy); writes = (5, 4) to (5, 7): (some property)}
        }]
        |}]

let%expect_test "logic_op_assign_repeat" =
  print_ssa_test {|
function member_op_assignment_refinement_ok(o: {p: ?number}) {
  o.p &&= 3;
  o.p &&= 3;
  o.p &&= 3;
}
|};
    [%expect {|
      [
        (3, 2) to (3, 3) => {
          (2, 44) to (2, 45): (`o`)
        };
        (4, 2) to (4, 3) => {
          (2, 44) to (2, 45): (`o`)
        };
        (4, 2) to (4, 5) => {
          (3, 2) to (3, 5): (some property)
        };
        (5, 2) to (5, 3) => {
          (2, 44) to (2, 45): (`o`)
        };
        (5, 2) to (5, 5) => {
          (4, 2) to (4, 5): (some property)
        }]
        |}]

let%expect_test "logic_op_assign_repeat_existing_refi" =
  print_ssa_test {|
function member_op_assignment_refinement_ok(o: {p: ?number}) {
  if (o.p === 0) {
    o.p ??= 3;
    o.p &&= (o.p && 3);
    o.p ||= (o.p && 3);
  }
}
|};
    [%expect {|
      [
        (3, 6) to (3, 7) => {
          (2, 44) to (2, 45): (`o`)
        };
        (4, 4) to (4, 5) => {
          {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}
        };
        (4, 4) to (4, 7) => {
          {refinement = 0; writes = projection at (3, 6) to (3, 9)}
        };
        (5, 4) to (5, 5) => {
          {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}
        };
        (5, 4) to (5, 7) => {
          (4, 4) to (4, 7): (some property)
        };
        (5, 13) to (5, 14) => {
          {refinement = PropTruthyR (p); writes = {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}}
        };
        (5, 13) to (5, 16) => {
          {refinement = Truthy; writes = (4, 4) to (4, 7): (some property)}
        };
        (6, 4) to (6, 5) => {
          {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}
        };
        (6, 4) to (6, 7) => {
          (5, 4) to (5, 7): (some property)
        };
        (6, 13) to (6, 14) => {
          {refinement = Not (PropTruthyR (p)); writes = {refinement = SentinelR p; writes = (2, 44) to (2, 45): (`o`)}}
        };
        (6, 13) to (6, 16) => {
          {refinement = Not (Truthy); writes = (5, 4) to (5, 7): (some property)}
        }]
        |}]

let%expect_test "vals" =
  print_ssa_test {|
declare var values: mixed;

if (values.bxxxx === values.axxxx) {
  values;
  values.axxxx;
  values.bxxxx;
}
|};
    [%expect {|
      [
        (4, 4) to (4, 10) => {
          (2, 12) to (2, 18): (`values`)
        };
        (4, 21) to (4, 27) => {
          (2, 12) to (2, 18): (`values`)
        };
        (5, 2) to (5, 8) => {
          {refinement = SentinelR bxxxx; writes = (2, 12) to (2, 18): (`values`)}
        };
        (6, 2) to (6, 8) => {
          {refinement = SentinelR bxxxx; writes = (2, 12) to (2, 18): (`values`)}
        };
        (7, 2) to (7, 8) => {
          {refinement = SentinelR bxxxx; writes = (2, 12) to (2, 18): (`values`)}
        }]
        |}]

let%expect_test "destruct_default" =
  print_ssa_test {|
let x = 42;
let y;

({x = x, y = x} = {});
([x = x, y = x] = []);
|};
    [%expect {|
      [
        (5, 6) to (5, 7) => {
          (2, 4) to (2, 5): (`x`)
        };
        (5, 13) to (5, 14) => {
          (5, 2) to (5, 3): (`x`)
        };
        (6, 6) to (6, 7) => {
          (5, 2) to (5, 3): (`x`)
        };
        (6, 13) to (6, 14) => {
          (6, 2) to (6, 3): (`x`)
        }]
        |}]

let%expect_test "component_declaration" =
  print_ssa_test {|
  Foo;
  component Foo(param: T) { param }
  Foo;
|};
    [%expect {|
      [
        (2, 2) to (2, 5) => {
          (3, 12) to (3, 15): (`Foo`)
        };
        (3, 23) to (3, 24) => {
          Global T
        };
        (3, 28) to (3, 33) => {
          (3, 16) to (3, 21): (`param`)
        };
        (4, 2) to (4, 5) => {
          (3, 12) to (3, 15): (`Foo`)
        }]
        |}]

let%expect_test "component_declaration_conflicting_defs" =
  print_ssa_test {|
  component Foo(param: T) {
    const param = 1;
    param
  }
|};
    [%expect {|
      [
        (2, 23) to (2, 24) => {
          Global T
        };
        (4, 4) to (4, 9) => {
          (2, 16) to (2, 21): (`param`)
        }]
        |}]

let%expect_test "dead_component_declaration" =
  print_ssa_test {|
  throw 'lol';
  component Foo(param: T) { param } // hoisted
  Foo;
|};
    [%expect{|
      [
        (3, 23) to (3, 24) => {
          Global T
        };
        (3, 28) to (3, 33) => {
          (3, 16) to (3, 21): (`param`)
        };
        (4, 2) to (4, 5) => {
          unreachable
        }] |}]

let%expect_test "component_refinement_scope" =
  print_ssa_test {|
  let x = null;
  invariant(x == null);
  x; // refined
  component Foo() {
    x // not refined
  }
|};
    [%expect{|
      [
        (3, 2) to (3, 11) => {
          Global invariant
        };
        (3, 12) to (3, 13) => {
          (2, 6) to (2, 7): (`x`)
        };
        (4, 2) to (4, 3) => {
          {refinement = Not (Not (Maybe)); writes = (2, 6) to (2, 7): (`x`)}
        };
        (6, 4) to (6, 5) => {
          (2, 6) to (2, 7): (`x`)
        }] |}]

let%expect_test "component_param_scoping" =
  print_ssa_test {|
  component Foo(
    x: number,
    y: typeof x, // x not in scope, so global
    z = y, // y not in scope, so global
  ) {
  }
|};
    [%expect{|
      [
        (4, 14) to (4, 15) => {
          Global x
        };
        (5, 8) to (5, 9) => {
          Global y
        }] |}]

let%expect_test "type_guard_scoping" =
  print_ssa_test {|
  const x = 1;
  function f(x: mixed): x is number {
    return typeof x === "number";
  }
|};
  [%expect{|
      [
        (3, 24) to (3, 25) => {
          (3, 13) to (3, 14): (`x`)
        };
        (4, 18) to (4, 19) => {
          (3, 13) to (3, 14): (`x`)
        }] |}]

let%expect_test "declare_component" =
  print_ssa_test {|
  const x = 42;
  declare component Foo(x: number, y: x);
  x;
  Foo;
|};
    [%expect{|
      [
        (3, 38) to (3, 39) => {
          (2, 8) to (2, 9): (`x`)
        };
        (4, 2) to (4, 3) => {
          (2, 8) to (2, 9): (`x`)
        };
        (5, 2) to (5, 5) => {
          (3, 20) to (3, 23): (`Foo`)
        }] |}]

let%expect_test "declare_component2" =
  print_ssa_test {|
declare component Y();

component X() {
    return <Y />;
}

<Y />;
|};
    [%expect{|
      [
        (5, 11) to (5, 16) => {
          Global React
        };
        (5, 12) to (5, 13) => {
          (2, 18) to (2, 19): (`Y`)
        };
        (8, 0) to (8, 5) => {
          Global React
        };
        (8, 1) to (8, 2) => {
          (2, 18) to (2, 19): (`Y`)
        }] |}]

let%expect_test "lowercase jsx" =
  print_ssa_test {|
  const div = 42;
  <div />;
  <floop />;
|};
  [%expect{|
      [
        (3, 2) to (3, 9) => {
          Global React
        };
        (3, 3) to (3, 6) => {
          (2, 8) to (2, 11): (`div`)
        };
        (4, 2) to (4, 11) => {
          Global React
        };
        (4, 3) to (4, 8) => {
          Global floop
        }] |}]

let%expect_test "component forwardref" =
  print_ssa_test {|
const React = require('react');
component Foo(ref: any) { } // should read
declare component Foo(ref: any); // should not read
  |};
    [%expect {|
      [
        (2, 14) to (2, 21) => {
          Global require
        };
        (3, 14) to (3, 17) => {
          (2, 6) to (2, 11): (`React`)
        }] |}]

let%expect_test "component type alias" =
  print_ssa_test {|
component Foo() {
    type Bar = string;
    const b: Bar = 'hi';
    return <div />;
}
  |};
    [%expect {|
      [
        (4, 13) to (4, 16) => {
          (3, 9) to (3, 12): (`Bar`)
        };
        (5, 11) to (5, 18) => {
          Global React
        };
        (5, 12) to (5, 15) => {
          Global div
        }] |}]

let%expect_test "nullish props" =
  print_ssa_test {|
if (x.y == null) {
  x;
  x.y;
}

if (x.y == undefined) {
  x;
  x.y;
}
  |};
    [%expect {|
      [
        (2, 4) to (2, 5) => {
          Global x
        };
        (3, 2) to (3, 3) => {
          {refinement = Not (Not (PropNullishR y)); writes = Global x}
        };
        (4, 2) to (4, 3) => {
          {refinement = Not (Not (PropNullishR y)); writes = Global x}
        };
        (4, 2) to (4, 5) => {
          {refinement = Not (Not (Maybe)); writes = projection at (2, 4) to (2, 7)}
        };
        (7, 4) to (7, 5) => {
          Global x
        };
        (7, 11) to (7, 20) => {
          Global undefined
        };
        (8, 2) to (8, 3) => {
          {refinement = Not (Not (PropNullishR y)); writes = Global x}
        };
        (9, 2) to (9, 3) => {
          {refinement = Not (Not (PropNullishR y)); writes = Global x}
        };
        (9, 2) to (9, 5) => {
          {refinement = Not (Not (Maybe)); writes = projection at (7, 4) to (7, 7)}
        }] |}]

let%expect_test "component type alias" =
  print_ssa_test {|
import * as React from 'react';
import {useRef} from 'react';

component Component() {
  const ref1 = useRef<?number>(null);
  if (ref1.current === null) {
    ref1.current = 42; // ok
  }
  ref1.current; // error

  const ref2 = useRef<?number>(null);
  if (ref2.current === null) {
    ref2.current; // error
  }

  const ref3 = useRef<?number>(null);
  if (ref3.current === null && ref1) {
    ref3.current = 42; // ok
  }

  const ref4 = useRef<?number>(null);
  if (ref4.current === null || ref1) {
    ref4.current = 42; // error
  }

  const ref5 = useRef<?number>(null);
  if (ref5.current == undefined) {
    ref5.current = 42; // ok
  }

  const ref6 = useRef<?number>(null);
  if (!ref6.current) {
    ref6.current = 42; // ok
  }
  return null;
}

  |};
    [%expect {|
      [
        (6, 15) to (6, 21) => {
          (3, 8) to (3, 14): (`useRef`)
        };
        (7, 6) to (7, 10) => {
          (6, 8) to (6, 12): (`ref1`)
        };
        (8, 4) to (8, 8) => {
          {refinement = SentinelR current; writes = (6, 8) to (6, 12): (`ref1`)}
        };
        (10, 2) to (10, 6) => {
          (6, 8) to (6, 12): (`ref1`)
        };
        (10, 2) to (10, 14) => {
          (8, 4) to (8, 16): (some property),
          {refinement = Not (Null); writes = projection at (7, 6) to (7, 18)}
        };
        (12, 15) to (12, 21) => {
          (3, 8) to (3, 14): (`useRef`)
        };
        (13, 6) to (13, 10) => {
          (12, 8) to (12, 12): (`ref2`)
        };
        (14, 4) to (14, 8) => {
          {refinement = SentinelR current; writes = (12, 8) to (12, 12): (`ref2`)}
        };
        (14, 4) to (14, 16) => {
          {refinement = Null; writes = projection at (13, 6) to (13, 18)}
        };
        (17, 15) to (17, 21) => {
          (3, 8) to (3, 14): (`useRef`)
        };
        (18, 6) to (18, 10) => {
          (17, 8) to (17, 12): (`ref3`)
        };
        (18, 31) to (18, 35) => {
          (6, 8) to (6, 12): (`ref1`)
        };
        (19, 4) to (19, 8) => {
          {refinement = SentinelR current; writes = (17, 8) to (17, 12): (`ref3`)}
        };
        (22, 15) to (22, 21) => {
          (3, 8) to (3, 14): (`useRef`)
        };
        (23, 6) to (23, 10) => {
          (22, 8) to (22, 12): (`ref4`)
        };
        (23, 31) to (23, 35) => {
          (6, 8) to (6, 12): (`ref1`)
        };
        (24, 4) to (24, 8) => {
          (22, 8) to (22, 12): (`ref4`)
        };
        (27, 15) to (27, 21) => {
          (3, 8) to (3, 14): (`useRef`)
        };
        (28, 6) to (28, 10) => {
          (27, 8) to (27, 12): (`ref5`)
        };
        (28, 22) to (28, 31) => {
          Global undefined
        };
        (29, 4) to (29, 8) => {
          {refinement = Not (Not (PropNullishR current)); writes = (27, 8) to (27, 12): (`ref5`)}
        };
        (32, 15) to (32, 21) => {
          (3, 8) to (3, 14): (`useRef`)
        };
        (33, 7) to (33, 11) => {
          (32, 8) to (32, 12): (`ref6`)
        };
        (34, 4) to (34, 8) => {
          {refinement = Not (PropTruthyR (current)); writes = (32, 8) to (32, 12): (`ref6`)}
        }] |}]

let%expect_test "strict_eq_ident" =
  print_ssa_test {|
  if (x === y) {
    x;
  } else {
    x;
  }
|};
  [%expect {|
    [
      (2, 6) to (2, 7) => {
        Global x
      };
      (2, 12) to (2, 13) => {
        Global y
      };
      (3, 4) to (3, 5) => {
        {refinement = EqR; writes = Global x}
      };
      (5, 4) to (5, 5) => {
        {refinement = Not (EqR); writes = Global x}
      }]
  |}]

let%expect_test "strict_eq_member" =
  print_ssa_test {|
  if (x === y.foo) {
    x;
  } else {
    x;
  }
|};
  [%expect {|
    [
      (2, 6) to (2, 7) => {
        Global x
      };
      (2, 12) to (2, 13) => {
        Global y
      };
      (3, 4) to (3, 5) => {
        {refinement = EqR; writes = Global x}
      };
      (5, 4) to (5, 5) => {
        {refinement = Not (EqR); writes = Global x}
      }]
  |}]

let%expect_test "match_object_pattern" =
  print_ssa_test {|
(match (x) {
  {type: 'foo', value: const a} => a as number,
  {type: 'bar'} => 1,
});
|};
  [%expect {|
    [
      (2, 1) to (2, 6) => {
        {refinement = Or (Not (And (object, Not (Null))), Not (SentinelR type)); writes = {refinement = Or (Or (Not (And (object, Not (Null))), Not (SentinelR type)), Not (PropExistsR (value))); writes = (2, 1) to (2, 6): (`<match_root>`)}}
      };
      (2, 8) to (2, 9) => {
        Global x
      };
      (3, 2) to (3, 47) => {
        {refinement = And (And (And (object, Not (Null)), SentinelR type), PropExistsR (value)); writes = (2, 1) to (2, 6): (`<match_root>`)}
      };
      (3, 35) to (3, 36) => {
        (3, 29) to (3, 30): (`a`)
      };
      (4, 2) to (4, 21) => {
        {refinement = And (And (object, Not (Null)), SentinelR type); writes = {refinement = Or (Or (Not (And (object, Not (Null))), Not (SentinelR type)), Not (PropExistsR (value))); writes = (2, 1) to (2, 6): (`<match_root>`)}}
      }]
  |}]

let%expect_test "match_array_pattern" =
  (* Test case aligned with object pattern test case above. *)
  print_ssa_test {|
(match (x) {
  [      'foo',        const a] => a as number,
  [      'bar'] => 1,
});
|};
  [%expect {|
    [
      (2, 1) to (2, 6) => {
        {refinement = Or (Not (And (isArray, array length === 1)), Not (SentinelR 0)); writes = {refinement = Or (Not (And (isArray, array length === 2)), Not (SentinelR 0)); writes = (2, 1) to (2, 6): (`<match_root>`)}}
      };
      (2, 8) to (2, 9) => {
        Global x
      };
      (3, 2) to (3, 47) => {
        {refinement = And (And (isArray, array length === 2), SentinelR 0); writes = (2, 1) to (2, 6): (`<match_root>`)}
      };
      (3, 35) to (3, 36) => {
        (3, 29) to (3, 30): (`a`)
      };
      (4, 2) to (4, 21) => {
        {refinement = And (And (isArray, array length === 1), SentinelR 0); writes = {refinement = Or (Not (And (isArray, array length === 2)), Not (SentinelR 0)); writes = (2, 1) to (2, 6): (`<match_root>`)}}
      }]
  |}]
