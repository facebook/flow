(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests =
  "ty_printer"
  >::: [
         ( "type_object_property_get" >:: fun ctxt ->
           let getter =
             Ty.NamedProp
               { name = "foo"; prop = Ty.Get (Ty.Str None); from_proto = false; def_loc = None }
           in
           let obj =
             Ty.Obj
               {
                 Ty.obj_kind = Ty.ExactObj;
                 obj_frozen = false;
                 obj_literal = None;
                 obj_props = [getter];
               }
           in
           let str = Ty_printer.string_of_t ~exact_by_default:true obj in
           assert_equal ~ctxt ~printer:(fun x -> x) "{get foo(): string}" str );
         ( "type_object_property_set" >:: fun ctxt ->
           let setter =
             Ty.NamedProp
               { name = "foo"; prop = Ty.Set (Ty.Str None); from_proto = false; def_loc = None }
           in
           let obj =
             Ty.Obj
               {
                 Ty.obj_kind = Ty.ExactObj;
                 obj_frozen = false;
                 obj_literal = None;
                 obj_props = [setter];
               }
           in
           let str = Ty_printer.string_of_t ~exact_by_default:true obj in
           assert_equal ~ctxt ~printer:(fun x -> x) "{set foo(string): void}" str );
       ]
