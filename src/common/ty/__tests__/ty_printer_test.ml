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
                 prop = Ty.Get Ty.Str;
                 inherited = false;
                 source = Ty.Other;
                 def_locs = [];
               }
           in
           let obj =
             Ty.Obj { Ty.obj_kind = Ty.ExactObj; obj_def_loc = None; obj_props = [getter] }
           in
           let str = Ty_printer.string_of_t obj in
           assert_equal ~ctxt ~printer:(fun x -> x) "{get foo(): string}" str
         );
         ( "type_object_property_set" >:: fun ctxt ->
           let setter =
             Ty.NamedProp
               {
                 name = Reason.OrdinaryName "foo";
                 prop = Ty.Set Ty.Str;
                 inherited = false;
                 source = Ty.Other;
                 def_locs = [];
               }
           in
           let obj =
             Ty.Obj { Ty.obj_kind = Ty.ExactObj; obj_def_loc = None; obj_props = [setter] }
           in
           let str = Ty_printer.string_of_t obj in
           assert_equal ~ctxt ~printer:(fun x -> x) "{set foo(string): void}" str
         );
         ( "empty_inexact_tuple" >:: fun ctxt ->
           let tup = Ty.Tup { elements = []; inexact = true } in
           let str = Ty_printer.string_of_t tup in
           assert_equal ~ctxt ~printer:(fun x -> x) "[...]" str
         );
         ( "inexact_tuple" >:: fun ctxt ->
           let elements =
             [Ty.TupleElement { name = None; t = Ty.Num; polarity = Ty.Neutral; optional = false }]
           in
           let tup = Ty.Tup { elements; inexact = true } in
           let str = Ty_printer.string_of_t tup in
           assert_equal ~ctxt ~printer:(fun x -> x) "[number, ...]" str
         );
         ( "top_type" >:: fun ctxt ->
           let str = Ty_printer.string_of_t Ty.Top in
           assert_equal ~ctxt ~printer:(fun x -> x) "unknown" str
         );
         ( "variance_keywords" >:: fun ctxt ->
           let mk_field name polarity t =
             Ty.NamedProp
               {
                 name = Reason.OrdinaryName name;
                 prop = Ty.Field { t; polarity; optional = false };
                 inherited = false;
                 source = Ty.Other;
                 def_locs = [];
               }
           in
           let obj =
             Ty.Obj
               {
                 Ty.obj_kind =
                   Ty.IndexedObj
                     {
                       Ty.dict_polarity = Ty.Positive;
                       dict_name = Some "key";
                       dict_key = Ty.Str;
                       dict_value = Ty.Top;
                     };
                 obj_def_loc = None;
                 obj_props = [mk_field "ro" Ty.Positive Ty.Str; mk_field "wo" Ty.Negative Ty.Num];
               }
           in
           let str = Ty_printer.string_of_t obj in
           assert_equal
             ~ctxt
             ~printer:(fun x -> x)
             "{readonly [key: string]: unknown, readonly ro: string, writeonly wo: number}"
             str
         );
         ( "type_param_bound_and_variance" >:: fun ctxt ->
           let func =
             Ty.Fun
               {
                 Ty.fun_params = [];
                 fun_rest_param = None;
                 fun_return = Ty.ReturnType Ty.Void;
                 fun_type_params =
                   Some
                     [
                       {
                         Ty.tp_name = "T";
                         tp_bound = Some Ty.Top;
                         tp_polarity = Ty.Positive;
                         tp_default = None;
                         tp_const = false;
                       };
                     ];
                 fun_static = Ty.Top;
                 fun_effect = Ty.Arbitrary;
               }
           in
           let str = Ty_printer.string_of_t func in
           assert_equal ~ctxt ~printer:(fun x -> x) "<out T extends unknown>() => void" str
         );
       ]
