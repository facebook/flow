(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Ty_symbol
include Ty_ancestors

type aloc = (ALoc.t[@printer (fun fmt loc -> fprintf fmt "%s" (ALoc.to_string_no_source loc))])
[@@deriving show]

exception Difference of int

(* To avoid VisitorsRuntime.StructuralMismatch exceptions when using comparator_ty,
 * we override the VisitorsRuntime.fail and raise `Difference 1` instead.
 *
 * Note that we still need to override the respective fail_* method for every Ty.t
 * variant type to get the most accurate results. To ensure that all such methods
 * have been overridden, check the file generated with
 *
 * ocamlfind ocamlopt \
 *  -ppx '\
 *    `ocamlfind query ppx_deriving`/ppx_deriving \
 *    `ocamlfind query -predicates ppx_driver,byte -format '%d/%a' ppx_deriving.show` \
 *    `ocamlfind query -predicates ppx_driver,byte -format '%d/%a' visitors.ppx`' \
 *  -dsource -c src/common/ty/ty.ml
 *
 * and make sure all fail_* methods in the iter_ty class are overridden in
 * comparator_ty.
 *)
module VisitorsRuntime = struct
  include VisitorsRuntime

  let fail () = raise (Difference 1)
end

type t =
  | Bound of aloc * string
  | Generic of generic_t
  | Any of any_kind
  | Top
  | Bot of bot_kind
  | Void
  | Null
  | Symbol
  | Num
  | Str
  | Bool
  | BigInt
  | NumLit of string
  | StrLit of Reason.name
  | BoolLit of bool
  | BigIntLit of string
  | Fun of fun_t
  | Obj of obj_t
  | Arr of arr_t
  | Tup of {
      elements: tuple_element list;
      inexact: bool;
    }
  | Union of bool (* from annotation *) * t * t * t list
  | Inter of t * t * t list
  | InlineInterface of interface_t
  | TypeOf of builtin_or_symbol * t list option
  | Utility of utility
  | IndexedAccess of {
      _object: t;
      index: t;
      optional: bool;
    }
  | Conditional of {
      check_type: t;
      extends_type: t;
      true_type: t;
      false_type: t;
    }
  | Infer of symbol * t option
  | Component of {
      regular_props: component_props;
      ref_prop: t option;
      renders: t option;
    }
  | Renders of t * renders_kind

(* Recursive variable *)
and generic_t = symbol * gen_kind * t list option

and any_kind =
  | Annotated of aloc
  | AnyError of any_error_kind option
  | Recursive
  | Unsound of unsoundness_kind
  | Untyped
  | Placeholder

and any_error_kind =
  | UnresolvedName
  | MissingAnnotation

and unsoundness_kind =
  | BoundFunctionThis
  | Constructor
  | DummyStatic
  | Exports
  | InferenceHooks
  | InstanceOfRefinement
  | Merged
  | ResolveSpread
  | Unchecked
  | Unimplemented
  | UnresolvedType
  | NonBindingPattern

(* The purpose of adding this distinction is to enable normalized types to mimic
 * the behavior of the signature optimizer when exporting types that contain
 * tvars with no lower bounds.
 *)
and upper_bound_kind =
  (* No upper bounds are exported as `any` *)
  | NoUpper
  (* Some upper bound use type. *)
  | SomeKnownUpper of t
  (* If the above case fails we resort to this last case. *)
  | SomeUnknownUpper of string

and bot_kind =
  (* Type.Empty *)
  | EmptyType
  (* A tvar with no lower bounds *)
  | NoLowerWithUpper of upper_bound_kind

and gen_kind =
  | ClassKind
  | InterfaceKind
  | TypeAliasKind
  | EnumKind
  | ComponentKind

and fun_effect =
  | Hook
  | Arbitrary

and fun_t = {
  fun_params: (string option * t * fun_param) list;
  fun_rest_param: (string option * t) option;
  fun_return: return_t;
  fun_type_params: type_param list option;
  fun_static: t;
  fun_effect: fun_effect;
}

and return_t =
  | ReturnType of t
  | TypeGuard of bool (* implies *) * string * t

and obj_kind =
  | ExactObj
  | InexactObj
  | IndexedObj of dict
  | MappedTypeObj

and obj_t = {
  obj_def_loc: aloc option;
  (* `None` means that this field was not computed, because the normalizer config
     option preserve_inferred_literal_types was set to false. `Some b` means that
     it was computed and `b` is true iff this is a literal type. *)
  obj_literal: bool option;
  obj_props: prop list;
  obj_kind: obj_kind;
}

and arr_t = {
  arr_readonly: bool;
  (* `None` means that this field was not computed, because the normalizer config
     option preserve_inferred_literal_types was set to false. `Some b` means that
     it was computed and `b` is true iff this is a literal type. *)
  arr_literal: bool option;
  arr_elt_t: t;
}

and tuple_element =
  | TupleElement of {
      name: string option;
      t: t;
      polarity: polarity;
      optional: bool;
    }
  | TupleSpread of {
      name: string option;
      t: t;
    }

and interface_t = {
  if_extends: generic_t list;
  if_props: prop list;
  if_dict: dict option;
}

and fun_param = { prm_optional: bool }

and prop =
  | NamedProp of {
      name: Reason.name;
      prop: named_prop;
      inherited: bool;
      source: prop_source;
      def_locs: aloc list;
    }
  | CallProp of fun_t
  | SpreadProp of t
  | MappedTypeProp of {
      key_tparam: type_param;
      source: t;
      prop: t;
      flags: mapped_type_flags;
      homomorphic: mapped_type_homomorphic_flag;
    }

and mapped_type_homomorphic_flag =
  | Homomorphic
  | SemiHomomorphic of t
  | Unspecialized

and named_prop =
  | Field of {
      t: t;
      polarity: polarity;
      optional: bool;
    }
  | Method of fun_t
  | Get of t
  | Set of t

and prop_source =
  | Interface
  | PrimitiveProto of string
  | Other

and mapped_type_optional_flag =
  | RemoveOptional
  | KeepOptionality
  | MakeOptional

and mapped_type_flags = {
  optional: mapped_type_optional_flag;
  polarity: polarity;
}

and dict = {
  dict_polarity: polarity;
  dict_name: string option;
  dict_key: t;
  dict_value: t;
}

and type_param = {
  tp_name: string;
  tp_bound: t option;
  tp_polarity: polarity;
  tp_default: t option;
  tp_const: bool;
}

and utility =
  (* https://flow.org/en/docs/types/utilities/ *)
  | Keys of t
  | Values of t
  | ReadOnly of t
  | Partial of t
  | Required of t
  | Exact of t
  | Omit of t * t
  | ElementType of t * t
  | Enum of t
  | NonMaybeType of t
  | ObjKeyMirror of t
  | Class of t
  | StringPrefix of {
      prefix: string;
      remainder: t option;
    }
  | StringSuffix of {
      suffix: string;
      remainder: t option;
    }
  (* React utils *)
  | ReactElementPropsType of t
  | ReactElementConfigType of t

and component_props =
  | UnflattenedComponentProps of t
  | FlattenedComponentProps of {
      props: flattened_component_prop list;
      inexact: bool;
    }

and flattened_component_prop =
  | FlattenedComponentProp of {
      name: Reason.name;
      optional: bool;
      def_locs: aloc list;
      t: t;
    }

and renders_kind =
  | RendersNormal
  | RendersMaybe
  | RendersStar

and polarity =
  | Positive
  | Negative
  | Neutral

and builtin_or_symbol =
  | FunProto
  | ObjProto
  | FunProtoBind
  | TSymbol of symbol

and decl =
  | VariableDecl of Reason.name * t
  | TypeAliasDecl of {
      import: bool;
      name: symbol;
      tparams: type_param list option;
      type_: t option;
    }
  | ClassDecl of symbol * type_param list option
  | InterfaceDecl of symbol * type_param list option
  | EnumDecl of symbol
  | NominalComponentDecl of {
      name: symbol;
      tparams: type_param list option;
      (* Used to show instantiation at JSX creation site. *)
      targs: t list option;
      props: component_props;
      instance: t option;
      renders: t option;
      is_type: bool;
    }
  | NamespaceDecl of {
      name: symbol option;
      exports: decl list;
    }
  | ModuleDecl of {
      name: symbol option;
      exports: decl list;
      default: t option;
    }

and elt =
  | Type of t
  | Decl of decl
[@@deriving
  visitors
    {
      name = "iter_ty";
      nude = true;
      variety = "iter";
      visit_prefix = "on_";
      ancestors = ["iter_ty_base"];
    },
    visitors
      {
        name = "iter2_ty";
        nude = true;
        variety = "iter2";
        visit_prefix = "on_";
        ancestors = ["iter2_ty_base"];
      },
    visitors
      {
        name = "reduce_ty";
        variety = "reduce";
        nude = true;
        visit_prefix = "on_";
        ancestors = ["reduce_ty_base"];
      },
    visitors
      {
        name = "map_ty";
        variety = "map";
        nude = true;
        visit_prefix = "on_";
        ancestors = ["map_ty_base"];
      },
    visitors
      {
        name = "endo_ty";
        variety = "endo";
        nude = true;
        visit_prefix = "on_";
        ancestors = ["endo_ty_base"];
      },
    visitors
      {
        name = "mapreduce_ty";
        variety = "mapreduce";
        nude = true;
        visit_prefix = "on_";
        ancestors = ["mapreduce_ty_base"];
      },
    show]

let assert0 i =
  if i == 0 then
    ()
  else
    raise (Difference i)

(* The prototype of what should happen when overriding fail_* methods *)
let fail_gen : 'env 'x. ('env -> 'x -> int) -> 'env -> 'x -> 'x -> unit =
 (fun tag_of env t1 t2 -> assert0 (tag_of env t1 - tag_of env t2))

(* Compare Ty.t for structural equality
   This class can be overridden to define new forms of equality on types *)
class ['A] comparator_ty =
  object (this)
    inherit [_] iter2_ty as super

    method compare (env : 'A) (t1 : t) (t2 : t) =
      try
        this#on_t env t1 t2;
        0
      with
      | Difference n -> n

    method compare_elt (env : 'A) (elt1 : elt) (elt2 : elt) =
      try
        this#on_elt env elt1 elt2;
        0
      with
      | Difference n -> n

    (* Take advantage of pointer equality at type nodes to short circuit *)
    method! private on_t env x y =
      if x == y then
        ()
      else
        super#on_t env x y

    (* When comparing NamedProps ignore def_loc, because properties having different
     * def_locs doesn't mean that they should be thought of as different *)
    method! private on_NamedProp
        env
        (name_0 : Reason.name)
        (name_1 : Reason.name)
        (prop_0 : named_prop)
        (prop_1 : named_prop)
        (inherited_0 : bool)
        (inherited_1 : bool)
        (source_0 : prop_source)
        (source_1 : prop_source)
        (_def_locs_0 : aloc list)
        (_def_locs_1 : aloc list) =
      super#on_NamedProp
        env
        name_0
        name_1
        prop_0
        prop_1
        inherited_0
        inherited_1
        source_0
        source_1
        []
        []

    method! on_obj_t env obj_1 obj_2 =
      super#on_obj_t env { obj_1 with obj_def_loc = None } { obj_2 with obj_def_loc = None }

    method! on_name env name0 name1 =
      (* TODO consider implementing this without the string conversion. For now, leaving it this
       * way to avoid a behavior change. *)
      this#on_string env (Reason.display_string_of_name name0) (Reason.display_string_of_name name1)

    (* Base fields originally handled in the ancestor *)
    method! private on_int _env x y = assert0 (x - y)

    method! private on_string env x y =
      (* In order to sort integer literals we try to parse all strings as integers *)
      match int_of_string x with
      | x -> begin
        match int_of_string y with
        (* If both parse as integers then we compare them as integers *)
        | y -> this#on_int env x y
        (* If xor parses as an integer then that one is "less than" the other *)
        | exception Failure _ -> raise (Difference (-1))
      end
      | exception Failure _ -> begin
        match int_of_string y with
        | _ -> raise (Difference 1)
        (* If neither parse as integers then we compare them as strings *)
        | exception Failure _ -> assert0 (String.compare x y)
      end

    method! private on_bool _env x y = assert0 (Stdlib.compare x y)

    method! private on_symbol _env x y = assert0 (Stdlib.compare x y)

    method! private on_aloc _env x y = assert0 (ALoc.compare x y)

    method! private fail_option _env x _y =
      match x with
      | None -> raise (Difference (-1))
      | _ -> raise (Difference 1)

    method! private fail_list _env x _y =
      match x with
      | [] -> raise (Difference (-1))
      | _ -> raise (Difference 1)

    (* This class must override all fail_* methods on variant types to be correct. *)
    (* The following methods are ordered respectively with the
       definitions in this file to make it easier to check *)
    method! private fail_t env x y = fail_gen this#tag_of_t env x y

    method! private fail_any_kind env x y = fail_gen this#tag_of_any_kind env x y

    method! private fail_upper_bound_kind env x y = fail_gen this#tag_of_upper_bound_kind env x y

    method! private fail_bot_kind env x y = fail_gen this#tag_of_bot_kind env x y

    method! private fail_gen_kind env x y = fail_gen this#tag_of_gen_kind env x y

    method! private fail_obj_kind env x y = fail_gen this#tag_of_obj_kind env x y

    method! private fail_tuple_element env x y = fail_gen this#tag_of_tuple_element env x y

    method! private fail_prop env x y = fail_gen this#tag_of_prop env x y

    method! private fail_named_prop env x y = fail_gen this#tag_of_named_prop env x y

    method! private fail_prop_source env x y = fail_gen this#tag_of_prop_source env x y

    method! private fail_utility env x y = fail_gen this#tag_of_utility env x y

    method! private fail_polarity env x y = fail_gen this#tag_of_polarity env x y

    method! private fail_unsoundness_kind env x y = fail_gen this#tag_of_unsoundness_kind env x y

    method! private fail_builtin_or_symbol env x y = fail_gen this#tag_of_builtin_or_symbol env x y

    method! private fail_decl env x y = fail_gen this#tag_of_decl env x y

    method! private fail_elt env x y = fail_gen this#tag_of_elt env x y

    method! private fail_return_t env x y = fail_gen this#tag_of_return_t env x y

    method! private fail_any_error_kind env x y = fail_gen this#tag_of_any_error_kind env x y

    method! private fail_mapped_type_homomorphic_flag env x y =
      fail_gen this#tag_of_mapped_type_homomorphic_flag env x y

    method! private fail_mapped_type_optional_flag env x y =
      fail_gen this#tag_of_mapped_type_optional_flag env x y

    (* types will show up in unions and intersections in ascending order *)
    (* No two elements of each variant can be assigned the same tag *)
    method tag_of_t _ =
      function
      (* Roughly in order of increasing complexity *)
      (* Favor litererals over base types *)
      (* Favor user defined types over structural types *)
      | Bot _ -> 0
      | Top -> 1
      | Any _ -> 2
      | Void -> 3
      | Null -> 4
      | BoolLit _ -> 5
      | Bool -> 6
      | NumLit _ -> 7
      | Num -> 8
      | BigIntLit _ -> 9
      | BigInt -> 10
      | StrLit _ -> 11
      | Str -> 12
      | Symbol -> 13
      | Bound _ -> 15
      | Generic _ -> 16
      | TypeOf _ -> 17
      | Utility _ -> 18
      | IndexedAccess _ -> 19
      | Tup _ -> 20
      | Arr _ -> 21
      | Fun _ -> 22
      | Obj _ -> 23
      | Inter _ -> 24
      | Union _ -> 25
      | InlineInterface _ -> 26
      | Conditional _ -> 27
      | Infer _ -> 28
      | Component _ -> 29
      | Renders _ -> 30

    method tag_of_decl _ =
      function
      | VariableDecl _ -> 0
      | TypeAliasDecl _ -> 1
      | ClassDecl _ -> 2
      | InterfaceDecl _ -> 3
      | EnumDecl _ -> 4
      | NominalComponentDecl _ -> 5
      | NamespaceDecl _ -> 6
      | ModuleDecl _ -> 7

    method tag_of_elt _ =
      function
      | Type _ -> 0
      | Decl _ -> 1

    method tag_of_gen_kind _ =
      function
      | ClassKind -> 0
      | InterfaceKind -> 1
      | TypeAliasKind -> 2
      | EnumKind -> 3
      | ComponentKind -> 4

    method tag_of_obj_kind _ =
      function
      | ExactObj -> 0
      | InexactObj -> 1
      | IndexedObj _ -> 2
      | MappedTypeObj -> 3

    method tag_of_tuple_element _ =
      function
      | TupleElement _ -> 0
      | TupleSpread _ -> 1

    method tag_of_any_kind _ =
      function
      | Annotated _ -> 0
      | AnyError _ -> 1
      | Recursive -> 2
      | Unsound _ -> 3
      | Untyped -> 4
      | Placeholder -> 5

    method tag_of_unsoundness_kind _ =
      function
      | BoundFunctionThis -> 0
      | Constructor -> 2
      | DummyStatic -> 3
      | Exports -> 5
      | InferenceHooks -> 7
      | InstanceOfRefinement -> 8
      | Merged -> 9
      | ResolveSpread -> 10
      | Unchecked -> 11
      | Unimplemented -> 12
      | UnresolvedType -> 13
      | NonBindingPattern -> 14

    method tag_of_prop _env =
      function
      | NamedProp _ -> 0
      | CallProp _ -> 1
      | SpreadProp _ -> 2
      | MappedTypeProp _ -> 3

    method tag_of_named_prop _env =
      function
      | Field _ -> 0
      | Method _ -> 1
      | Get _ -> 2
      | Set _ -> 3

    method tag_of_prop_source _env =
      function
      | Interface -> 0
      | PrimitiveProto _ -> 1
      | Other -> 2

    method tag_of_utility _ =
      function
      | Keys _ -> 0
      | Values _ -> 1
      | ReadOnly _ -> 2
      | Exact _ -> 3
      | ElementType _ -> 7
      | NonMaybeType _ -> 8
      | Class _ -> 13
      | ReactElementPropsType _ -> 18
      | ReactElementConfigType _ -> 19
      | ObjKeyMirror _ -> 22
      | Partial _ -> 23
      | Required _ -> 24
      | Enum _ -> 27
      | StringPrefix _ -> 28
      | StringSuffix _ -> 29
      | Omit _ -> 30

    method tag_of_polarity _ =
      function
      | Positive -> 0
      | Negative -> 1
      | Neutral -> 2

    method tag_of_bot_kind _env =
      function
      | EmptyType -> 0
      | NoLowerWithUpper _ -> 1

    method tag_of_upper_bound_kind _env =
      function
      | NoUpper -> 0
      | SomeKnownUpper _ -> 1
      | SomeUnknownUpper _ -> 2

    method tag_of_builtin_or_symbol _ =
      function
      | FunProto -> 0
      | ObjProto -> 1
      | FunProtoBind -> 2
      | TSymbol _ -> 3

    method tag_of_return_t _ =
      function
      | ReturnType _ -> 0
      | TypeGuard _ -> 1

    method tag_of_mapped_type_homomorphic_flag _ =
      function
      | Homomorphic -> 0
      | SemiHomomorphic _ -> 1
      | Unspecialized -> 2

    method tag_of_any_error_kind _ =
      function
      | UnresolvedName -> 0
      | MissingAnnotation -> 1

    method tag_of_mapped_type_optional_flag _ =
      function
      | RemoveOptional -> 0
      | KeepOptionality -> 1
      | MakeOptional -> 2
  end

(* Type destructors *)

let rec bk_union ?(flattened = false) = function
  | Union (_, t1, t2, ts) when flattened -> (t1, t2 :: ts)
  | Union (_, t1, t2, ts) -> Nel.map_concat bk_union (t1, t2 :: ts)
  | t -> (t, [])

let rec bk_inter ?(flattened = false) = function
  | Inter (t1, t2, ts) when flattened -> (t1, t2 :: ts)
  | Inter (t1, t2, ts) -> Nel.map_concat bk_inter (t1, t2 :: ts)
  | t -> (t, [])

(* Type constructors *)

let mk_union ~from_bounds ?(flattened = false) nel_ts =
  let (t, ts) = Nel.map_concat (bk_union ~flattened) nel_ts in
  match ts with
  | [] -> t
  | hd :: tl -> Union (from_bounds, t, hd, tl)

let mk_inter ?(flattened = false) nel_ts =
  let (t, ts) = Nel.map_concat (bk_inter ~flattened) nel_ts in
  match ts with
  | [] -> t
  | hd :: tl -> Inter (t, hd, tl)

let explicit_any = Any (Annotated ALoc.none)

let is_dynamic = function
  | Any _ -> true
  | _ -> false

let mk_maybe ~from_bounds t = mk_union ~from_bounds (Null, [Void; t])

let mk_generic_class symbol targs = Generic (symbol, ClassKind, targs)

let mk_generic_interface symbol targs = Generic (symbol, InterfaceKind, targs)

let mk_generic_talias symbol targs = Generic (symbol, TypeAliasKind, targs)

let mk_exact ty =
  match ty with
  | Obj o ->
    let obj_kind =
      match o.obj_kind with
      | InexactObj -> ExactObj
      | _ -> o.obj_kind
    in
    Obj { o with obj_kind }
  (* Not applicable *)
  | Any _
  | Top
  | Bot _
  | Void
  | Null
  | Symbol
  | Num
  | Str
  | Bool
  | BigInt
  | NumLit _
  | StrLit _
  | BoolLit _
  | BigIntLit _
  | Fun _
  | Arr _
  | Tup _
  | InlineInterface _
  | Infer _
  | Component _
  | Renders _ ->
    ty
  (* Do not nest $Exact *)
  | Utility (Exact _) -> ty
  (* Wrap in $Exact<...> *)
  | Generic _
  | Bound _
  | Union _
  | Inter _
  | TypeOf _
  | Utility _
  | IndexedAccess _
  | Conditional _ ->
    Utility (Exact ty)

let mk_array ~readonly ~literal t =
  Arr { arr_readonly = readonly; arr_literal = literal; arr_elt_t = t }

let debug_string_of_generic_kind = function
  | ClassKind -> "class"
  | InterfaceKind -> "interface"
  | TypeAliasKind -> "type alias"
  | EnumKind -> "enum"
  | ComponentKind -> "component"

let string_of_utility_ctor = function
  | Keys _ -> "$Keys"
  | Values _ -> "$Values"
  | ReadOnly _ -> "$ReadOnly"
  | Partial _ -> "Partial"
  | Required _ -> "Required"
  | Exact _ -> "$Exact"
  | Enum _ -> "Enum"
  | Omit _ -> "Omit"
  | ElementType _ -> "$ElementType"
  | NonMaybeType _ -> "$NonMaybeType"
  | ObjKeyMirror _ -> "$KeyMirror"
  | Class _ -> "Class"
  | StringPrefix _ -> "StringPrefix"
  | StringSuffix _ -> "StringSuffix"
  | ReactElementPropsType _ -> "React$ElementProps"
  | ReactElementConfigType _ -> "React$ElementConfig"

let types_of_utility = function
  | Keys t -> Some [t]
  | Values t -> Some [t]
  | ReadOnly t -> Some [t]
  | Partial t -> Some [t]
  | Required t -> Some [t]
  | Exact t -> Some [t]
  | Omit (t1, t2) -> Some [t1; t2]
  | ElementType (t1, t2) -> Some [t1; t2]
  | Enum t -> Some [t]
  | NonMaybeType t -> Some [t]
  | ObjKeyMirror t -> Some [t]
  | Class t -> Some [t]
  | StringPrefix { prefix = arg; remainder = None }
  | StringSuffix { suffix = arg; remainder = None } ->
    Some [StrLit (Reason.OrdinaryName arg)]
  | StringPrefix { prefix = arg; remainder = Some t }
  | StringSuffix { suffix = arg; remainder = Some t } ->
    Some [StrLit (Reason.OrdinaryName arg); t]
  | ReactElementPropsType t -> Some [t]
  | ReactElementConfigType t -> Some [t]

let string_of_prop_source = function
  | Interface -> "interface"
  | PrimitiveProto s -> s ^ ".prototype"
  | Other -> "other"

module LocSymbolSet : Flow_set.S with type elt = Loc.t symbol_ = struct
  module M = Flow_set.Make (struct
    type t = Loc.t symbol_

    let compare = Stdlib.compare
  end)

  include M
end

type type_at_pos_result = {
  unevaluated: elt;
  evaluated: elt option; (* Evaluation may not be possible *)
  refs: LocSymbolSet.t option; (* Use concrete locs for easier printing *)
}

let map_loc_remote_info ~f ri =
  let { imported_as } = ri in
  let imported_as =
    match imported_as with
    | Some (loc, name, mode) -> Some (f loc, name, mode)
    | None -> None
  in
  { imported_as }

let map_loc_provenance ~f sym_provenance =
  match sym_provenance with
  | Local -> Local
  | Builtin -> Builtin
  | Remote ri -> Remote (map_loc_remote_info ~f ri)
  | Library ri -> Library (map_loc_remote_info ~f ri)

let map_loc_symbol ~f (s : ALoc.t symbol_) : Loc.t symbol_ =
  let { sym_provenance; sym_def_loc; sym_name; sym_anonymous } = s in
  {
    sym_provenance = map_loc_provenance ~f sym_provenance;
    sym_def_loc = f sym_def_loc;
    sym_name;
    sym_anonymous;
  }

let symbols_of_elt ~loc_of_aloc =
  let singleton s = LocSymbolSet.singleton (map_loc_symbol ~f:loc_of_aloc s) in
  let o =
    object (_self)
      inherit [_] reduce_ty as _super

      method zero = LocSymbolSet.empty

      method plus = LocSymbolSet.union

      method! on_symbol _env s = singleton s
    end
  in
  function
  | Type t -> o#on_t () t
  | Decl (VariableDecl _ as d)
  | Decl (TypeAliasDecl _ as d)
  | Decl (ClassDecl _ as d)
  | Decl (InterfaceDecl _ as d)
  | Decl (EnumDecl _ as d)
  | Decl (NominalComponentDecl _ as d) ->
    o#on_decl () d
  | Decl (NamespaceDecl { name; exports = _ })
  | Decl (ModuleDecl { name; exports = _; default = _ }) ->
    Base.Option.value_map name ~default:LocSymbolSet.empty ~f:singleton
