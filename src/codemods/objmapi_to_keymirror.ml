(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module T = Ast.Type
module F = T.Function
module Id = Ast.Identifier
module TP = T.TypeParams
module GId = T.Generic.Identifier

module KeyMirrorStats = struct
  type t = {
    converted: int;
    inconvertible: int;
    illformed: int;
  }

  let empty = { converted = 0; inconvertible = 0; illformed = 0 }

  let combine x1 x2 =
    {
      converted = x1.converted + x2.converted;
      inconvertible = x1.inconvertible + x2.inconvertible;
      illformed = x1.illformed + x2.illformed;
    }

  let add_converted x = { x with converted = x.converted + 1 }

  let add_inconvertible x = { x with inconvertible = x.inconvertible + 1 }

  let add_illformed x = { x with illformed = x.illformed + 1 }

  let serialize x =
    [
      Utils_js.spf "converted: %d" x.converted;
      Utils_js.spf "inconvertible: %d" x.inconvertible;
      Utils_js.spf "illformed: %d" x.illformed;
    ]

  let report x =
    [
      Insert_type_utils.string_of_row ~indent:2 "Converted instances" x.converted;
      Insert_type_utils.string_of_row ~indent:2 "Not converted instances" x.inconvertible;
      Insert_type_utils.string_of_row ~indent:2 "Illformed instances" x.illformed;
    ]
end

module Acc = Insert_type_utils.UntypedAcc (KeyMirrorStats)

(* <K, ...>(k: K, ...) => K *)
let is_fst = function
  | ( _,
      T.Function
        {
          F.tparams =
            Some
              ( _,
                {
                  TP.params =
                    ( _,
                      {
                        T.TypeParam.name = (_, { Id.name = tparam_name; _ });
                        bound = T.Missing _;
                        variance = None;
                        default = None;
                      }
                    )
                    :: _;
                  _;
                }
              );
          params =
            ( _,
              {
                F.Params.this_ = None;
                params =
                  ( _,
                    {
                      F.Param.name = None;
                      annot =
                        ( _,
                          T.Generic
                            {
                              T.Generic.id = GId.Unqualified (_, { Id.name = param_name; _ });
                              targs = None;
                              _;
                            }
                        );
                      optional = false;
                    }
                  )
                  :: _;
                rest = None;
                _;
              }
            );
          return =
            ( _,
              T.Generic
                {
                  T.Generic.id = GId.Unqualified (_, { Id.name = return_name; _ });
                  targs = None;
                  _;
                }
            );
          _;
        }
    ) ->
    tparam_name = param_name && param_name = return_name
  | _ -> false

(* <K, ...>(k: K, ...) => T *)
let is_mappable = function
  | ( _,
      T.Function
        {
          F.tparams =
            Some
              ( _,
                {
                  TP.params =
                    ( _,
                      {
                        T.TypeParam.name = (_, { Id.name = tparam_name; _ });
                        bound = T.Missing _;
                        variance = None;
                        default = None;
                      }
                    )
                    :: _;
                  _;
                }
              );
          params =
            ( _,
              {
                F.Params.this_ = None;
                params =
                  ( _,
                    {
                      F.Param.name = None;
                      annot =
                        ( _,
                          T.Generic
                            {
                              T.Generic.id = GId.Unqualified (_, { Id.name = param_name; _ });
                              targs = None;
                              _;
                            }
                        );
                      optional = false;
                    }
                  )
                  :: _;
                rest = None;
                _;
              }
            );
          return = _;
          _;
        }
    ) ->
    tparam_name = param_name
  | _ -> false

let mapper ctx =
  object (this)
    inherit [Acc.t] Codemod_ast_mapper.mapper "" ~init:Acc.empty as super

    method! type_ t =
      let open T in
      let module G = Generic in
      let module GI = G.Identifier in
      let module I = Ast.Identifier in
      let t =
        match t with
        | ( loc,
            Generic
              {
                G.id = GI.Unqualified (id_loc, { I.name = "$ObjMapi"; comments = c1 });
                targs = Some (args_loc, { TypeArgs.arguments = [x; fn]; comments = c2 });
                comments = c3;
              }
          ) ->
          if is_fst fn then begin
            let extra = KeyMirrorStats.add_converted acc.Acc.stats in
            this#update_acc (fun acc -> Acc.update_stats acc extra);
            Hh_logger.info "Converted %s" (Reason.string_of_loc loc);
            ( loc,
              Generic
                {
                  G.id = GI.Unqualified (id_loc, { I.name = "$KeyMirror"; comments = c1 });
                  targs = Some (args_loc, { TypeArgs.arguments = [x]; comments = c2 });
                  comments = c3;
                }
            )
          end else if is_mappable fn then begin
            let extra = KeyMirrorStats.add_inconvertible acc.Acc.stats in
            this#update_acc (fun acc -> Acc.update_stats acc extra);
            Hh_logger.info "Skipping inconvertible %s" (Reason.string_of_loc loc);
            t
          end else
            let extra = KeyMirrorStats.add_inconvertible acc.Acc.stats in
            this#update_acc (fun acc -> Acc.update_stats acc extra);
            Hh_logger.info "Skipping inconvertible %s" (Reason.string_of_loc loc);
            t
        | (loc, Generic { G.id = GI.Unqualified (_, { I.name = "$ObjMapi"; _ }); targs = _; _ }) ->
          let extra = KeyMirrorStats.add_illformed acc.Acc.stats in
          this#update_acc (fun acc -> Acc.update_stats acc extra);
          Hh_logger.info "Skipping illformed %s" (Reason.string_of_loc loc);
          t
        | t -> t
      in
      super#type_ t

    method! program prog =
      let file = ctx.Codemod_context.Untyped.file in

      let prog' = super#program prog in

      if prog != prog' then
        this#update_acc (fun acc ->
            { acc with Acc.changed_set = Utils_js.FilenameSet.add file acc.Acc.changed_set }
        );
      prog'
  end
