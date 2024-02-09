(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ppxlib
open Ast_helper

(*

awk '/START OF PATTERN MATCH/,/END OF PATTERN MATCH/' path/to/flow_js.ml > input.ml

dune exec ./ppx_constraints_standalone.exe -- input.ml > output.ml
or
buck run //flow/scripts/ppx_constraints:ppx -- input.ml > output.ml

(optional)
ocamlformat -i --enable-outside-detected-project --margin 120 output.ml

Postprocess:

- Prefix wildcards:

cat output.ml | sed '/\(, \(_\|[a-z0-9]\+\) \(when cond \)\?-> ()\)/s/^/L-- /' | sed '/| \(_\|[a-z0-9]\+\), /s/^/--R /' | sed '/^|/s/^/    /'

*)

let rec do_ctor t =
  match t with
  | { ppat_desc = Ppat_construct ({ txt = Lident x; loc }, _); _ } ->
      {
        t with
        ppat_desc =
          Ppat_construct ({ txt = Lident x; loc }, Some ([], Pat.any ()));
      }
  | { ppat_desc = Ppat_or (p1, p2); _ } ->
      { t with ppat_desc = Ppat_or (do_ctor p1, do_ctor p2) }
  | _ -> t

let rec do_type t =
  match t with
  | {
   ppat_desc =
     Ppat_construct
       ( { txt = Lident "DefT"; loc = loc1 },
         Some (_, ({ ppat_desc = Ppat_tuple [ _; p2 ]; _ } as p1)) );
   _;
  } ->
      let p2' = do_ctor p2 in
      let p1' = { p1 with ppat_desc = Ppat_tuple [ Pat.any (); p2' ] } in
      {
        t with
        ppat_desc =
          Ppat_construct ({ txt = Lident "DefT"; loc = loc1 }, Some ([], p1'));
      }
  | { ppat_desc = Ppat_or (p1, p2); _ } ->
      { t with ppat_desc = Ppat_or (do_type p1, do_type p2) }
  | { ppat_desc = Ppat_construct ({ txt = Lident x; loc = loc1 }, _); _ } ->
      let p1' = Pat.any () in
      {
        t with
        ppat_desc =
          Ppat_construct ({ txt = Lident x; loc = loc1 }, Some ([], p1'));
      }
  | { ppat_desc = Ppat_alias (p, _); _ } -> do_type p
  | _ -> t

let rec do_use_t u =
  match u with
  | {
   ppat_desc =
     Ppat_construct
       ( ({ txt = Lident "PreprocessKitT"; _ } as id),
         Some
           ( _,
             ( {
                 ppat_desc =
                   Ppat_tuple
                     [
                       _;
                       ( {
                           ppat_desc =
                             Ppat_construct
                               (({ txt = Lident "ConcretizeTypes"; _ } as id2), Some (_, p3));
                           _;
                         } as p2
                       );
                     ];
                 _;
               } as p1
             )
           )
       );
   _;
  } ->
    let p3' = do_type p3 in
    let p1' =
      {
        p1 with
        ppat_desc =
          Ppat_tuple [Pat.any (); { p2 with ppat_desc = Ppat_construct (id2, Some ([], p3')) }];
      }
    in
    { u with ppat_desc = Ppat_construct (id, Some ([], p1')) }
  | {
   ppat_desc =
     Ppat_construct
       ( ({ txt = Lident ("UseT" | "PreprocessKitT"); _ } as id),
         Some (_, ({ ppat_desc = Ppat_tuple [_; p2]; _ } as p1))
       );
   _;
  } ->
    let p2' = do_type p2 in
    let p1' = { p1 with ppat_desc = Ppat_tuple [Pat.any (); p2'] } in
    { u with ppat_desc = Ppat_construct (id, Some ([], p1')) }
  | { ppat_desc = Ppat_construct ({ txt = Lident x; loc }, _); _ } ->
    { u with ppat_desc = Ppat_construct ({ txt = Lident x; loc }, Some ([], Pat.any ())) }
  | { ppat_desc = Ppat_or (p1, p2); _ } -> { u with ppat_desc = Ppat_or (do_use_t p1, do_use_t p2) }
  | { ppat_desc = Ppat_alias (p, _); _ } -> do_type p
  | _ -> u

let rec do_pattern pattern =
  let { ppat_desc; _ } = pattern in
  match ppat_desc with
  (* type, use *)
  | Ppat_tuple [ lhs; rhs ] ->
      [ { pattern with ppat_desc = Ppat_tuple [ do_type lhs; do_use_t rhs ] } ]
      (* type, type *)
  | Ppat_alias (p, _) -> do_pattern p
  | Ppat_or (p1, p2) -> do_pattern p1 @ do_pattern p2
  | _ -> [ pattern ]

let do_guard = function
  | None -> None
  | Some ({ pexp_loc = loc; _ } as e) ->
      Some
        {
          e with
          pexp_desc = Pexp_construct ({ txt = Lident "cond"; loc }, None);
        }

let do_case case =
  let { pc_lhs; pc_guard; pc_rhs } = case in
  let { pexp_loc = loc; _ } = pc_rhs in
  let pc_rhs =
    {
      pc_rhs with
      pexp_desc = Pexp_construct ({ txt = Lident "()"; loc }, None);
    }
  in
  let pc_lhs_list = do_pattern pc_lhs in
  List.map
    (fun pc_lhs -> { pc_lhs; pc_rhs; pc_guard = do_guard pc_guard })
    pc_lhs_list

let mapper =
  object
    inherit Ast_traverse.map as _super

    method! expression expr =
      match expr with
      | { pexp_desc = Pexp_match (m, cases); _ } as x ->
          let cases = List.map do_case cases |> List.concat in
          { x with pexp_desc = Pexp_match (m, cases) }
      | x -> x
  end

let () = Driver.register_transformation "ppx_constraints" ~impl:mapper#structure
