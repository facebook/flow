(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
               {
                 name = Reason.OrdinaryName "foo";
                 prop = Ty.Get (Ty.Str None);
                 inherited = false;
                 source = Ty.Other;
                 def_locs = [];
               }
           in
           let obj =
             Ty.Obj
               {
                 Ty.obj_kind = Ty.ExactObj;
                 obj_def_loc = None;
                 obj_literal = None;
                 obj_props = [getter];
               }
           in
           let str = Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false obj in
           assert_equal ~ctxt ~printer:(fun x -> x) "{get foo(): string}" str
         );
         ( "type_object_property_set" >:: fun ctxt ->
           let setter =
             Ty.NamedProp
               {
                 name = Reason.OrdinaryName "foo";
                 prop = Ty.Set (Ty.Str None);
                 inherited = false;
                 source = Ty.Other;
                 def_locs = [];
               }
           in
           let obj =
             Ty.Obj
               {
                 Ty.obj_kind = Ty.ExactObj;
                 obj_def_loc = None;
                 obj_literal = None;
                 obj_props = [setter];
               }
           in
           let str = Ty_printer.string_of_t ~exact_by_default:true ~ts_syntax:false obj in
           assert_equal ~ctxt ~printer:(fun x -> x) "{set foo(string): void}" str
         );
         ( "empty_inexact_tuple" >:: fun ctxt ->
           let tup = Ty.Tup { elements = []; inexact = true } in
           let str = Ty_printer.string_of_t ~ts_syntax:false tup in
           assert_equal ~ctxt ~printer:(fun x -> x) "[...]" str
         );
         ( "inexact_tuple" >:: fun ctxt ->
           let elements =
             [Ty.TupleElement { name = None; t = Ty.Num; polarity = Ty.Neutral; optional = false }]
           in
           let tup = Ty.Tup { elements; inexact = true } in
           let str = Ty_printer.string_of_t ~ts_syntax:false tup in
           assert_equal ~ctxt ~printer:(fun x -> x) "[number, ...]" str
         );
       ]
