(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Ty_symbol
include Ty_ancestors

type aloc = (ALoc.t[@printer (fun fmt loc -> fprintf fmt "%s" (ALoc.to_string_no_source loc))])
[@@deriving show]

(* WARNING to avoid VisitorsRuntime.StructuralMismatch exceptions when using
 * comparator_ty, make sure to override the respective fail_* method for every
 * variant type. To ensure that all such methods have been overridden, check the
 * file generated with
 *
 *  ocamlfind ppx_tools/rewriter \
 *    -ppx ' \
 *    `ocamlfind query ppx_deriving`/ppx_deriving \
 *    `ocamlfind query -predicates ppx_driver,byte -format '%d/%a' ppx_deriving.show` \
 *    `ocamlfind query -predicates ppx_driver,byte -format '%d/%a' visitors.ppx`' \
 *    src/common/ty/ty.ml
 *
 * and make sure all fail_* methods in the iter_ty class are overridden in
 * comparator_ty.
 *)
type t =
  | TVar of tvar * t list option
  | Bound of aloc * string
  | Generic of generic_t
  | Any of any_kind
  | Top
  | Bot of bot_kind
  | Void
  | Null
  | Symbol
  | Num of string option
  | Str of Reason.name option
  | Bool of bool option
  | NumLit of string
  | StrLit of Reason.name
  | BoolLit of bool
  | Fun of fun_t
  | Obj of obj_t
  | Arr of arr_t
  | Tup of t list
  | Union of bool (* from annotation *) * t * t * t list
  | Inter of t * t * t list
  | InlineInterface of interface_t
  | TypeOf of builtin_or_symbol
  | Utility of utility
  | Mu of int * t
  | CharSet of string
  | IndexedAccess of {
      _object: t;
      index: t;
      optional: bool;
    }

and tvar = RVar of int [@@unboxed]

(* Recursive variable *)
and generic_t = symbol * gen_kind * t list option

and any_kind =
  | Annotated of aloc
  | AnyError of any_error_kind option
  | Unsound of unsoundness_kind
  | Untyped

and any_error_kind = UnresolvedName

and unsoundness_kind =
  | BoundFunctionThis
  | ComputedNonLiteralKey
  | Constructor
  | DummyStatic
  | Exports
  | FunctionPrototype
  | InferenceHooks
  | InstanceOfRefinement
  | Merged
  | ResolveSpread
  | Unchecked
  | Unimplemented
  | UnresolvedType

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
  (* Type.MatchingPropT *)
  | EmptyMatchingPropT
  (* Type.TypeDestructorTriggerT *)
  | EmptyTypeDestructorTriggerT of aloc
  (* A tvar with no lower bounds *)
  | NoLowerWithUpper of upper_bound_kind

and gen_kind =
  | ClassKind
  | InterfaceKind
  | TypeAliasKind
  | EnumKind

and fun_t = {
  fun_params: (string option * t * fun_param) list;
  fun_rest_param: (string option * t) option;
  fun_return: t;
  fun_type_params: type_param list option;
  fun_static: t;
}

and obj_kind =
  | ExactObj
  | InexactObj
  | IndexedObj of dict

and obj_t = {
  obj_frozen: bool;
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
      from_proto: bool;
      def_loc: aloc option;
    }
  | CallProp of fun_t
  | SpreadProp of t

and named_prop =
  | Field of {
      t: t;
      polarity: polarity;
      optional: bool;
    }
  | Method of fun_t
  | Get of t
  | Set of t

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
}

and utility =
  (* https://flow.org/en/docs/types/utilities/ *)
  | Keys of t
  | Values of t
  | ReadOnly of t
  | Partial of t
  | Exact of t
  | Diff of t * t
  | Rest of t * t
  | ElementType of t * t
  | NonMaybeType of t
  | ObjMap of t * t
  | ObjMapi of t * t
  | ObjKeyMirror of t
  | ObjMapConst of t * t
  | TupleMap of t * t
  | Call of t * t list
  | Class of t
  | Shape of t
  (* React utils *)
  | ReactElementPropsType of t
  | ReactElementConfigType of t
  | ReactElementRefType of t
  | ReactConfigType of t * t

and polarity =
  | Positive
  | Negative
  | Neutral

and builtin_or_symbol =
  | FunProto
  | ObjProto
  | FunProtoApply
  | FunProtoBind
  | FunProtoCall
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

exception Difference of int

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

    (* Take advantage of pointer equality at type nodes to short circut *)
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
        (from_proto_0 : bool)
        (from_proto_1 : bool)
        (_def_loc_0 : aloc option)
        (_def_loc_1 : aloc option) =
      super#on_NamedProp env name_0 name_1 prop_0 prop_1 from_proto_0 from_proto_1 None None

    method! on_name env name0 name1 =
      (* TODO consider implementing this without the string conversion. For now, leaving it this
       * way to avoid a behavior change. *)
      this#on_string env (Reason.display_string_of_name name0) (Reason.display_string_of_name name1)

    (* Base fields originally handled in the ancestor *)
    method! private on_int _env x y = assert0 (x - y)

    method! private on_string env x y =
      (* In order to sort integer literals we try to parse all strings as integers *)
      match int_of_string x with
      | x ->
        begin
          match int_of_string y with
          (* If both parse as integers then we compare them as integers *)
          | y -> this#on_int env x y
          (* If xor parses as an integer then that one is "less than" the other *)
          | exception Failure _ -> raise (Difference (-1))
        end
      | exception Failure _ ->
        begin
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

    method! private fail_prop env x y = fail_gen this#tag_of_prop env x y

    method! private fail_named_prop env x y = fail_gen this#tag_of_named_prop env x y

    method! private fail_utility env x y = fail_gen this#tag_of_utility env x y

    method! private fail_polarity env x y = fail_gen this#tag_of_polarity env x y

    method! private fail_unsoundness_kind env x y = fail_gen this#tag_of_unsoundness_kind env x y

    method! private fail_builtin_or_symbol env x y = fail_gen this#tag_of_builtin_or_symbol env x y

    method! private fail_decl env x y = fail_gen this#tag_of_decl env x y

    method! private fail_elt env x y = fail_gen this#tag_of_elt env x y

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
      | Bool _ -> 6
      | NumLit _ -> 7
      | Num _ -> 8
      | StrLit _ -> 9
      | Str _ -> 10
      | Symbol -> 11
      | TVar _ -> 12
      | Bound _ -> 13
      | Generic _ -> 14
      | TypeOf _ -> 15
      | Utility _ -> 16
      | IndexedAccess _ -> 17
      | Tup _ -> 18
      | Arr _ -> 19
      | Fun _ -> 20
      | Obj _ -> 21
      | Inter _ -> 22
      | Union _ -> 23
      | Mu _ -> 24
      | InlineInterface _ -> 25
      | CharSet _ -> 26

    method tag_of_decl _ =
      function
      | VariableDecl _ -> 0
      | TypeAliasDecl _ -> 1
      | ClassDecl _ -> 2
      | InterfaceDecl _ -> 3
      | EnumDecl _ -> 4
      | ModuleDecl _ -> 5

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

    method tag_of_obj_kind _ =
      function
      | ExactObj -> 0
      | InexactObj -> 1
      | IndexedObj _ -> 2

    method tag_of_any_kind _ =
      function
      | Annotated _ -> 0
      | AnyError _ -> 1
      | Unsound _ -> 2
      | Untyped -> 3

    method tag_of_unsoundness_kind _ =
      function
      | BoundFunctionThis -> 0
      | ComputedNonLiteralKey -> 1
      | Constructor -> 2
      | DummyStatic -> 3
      | Exports -> 5
      | FunctionPrototype -> 6
      | InferenceHooks -> 7
      | InstanceOfRefinement -> 8
      | Merged -> 9
      | ResolveSpread -> 10
      | Unchecked -> 11
      | Unimplemented -> 12
      | UnresolvedType -> 13

    method tag_of_prop _env =
      function
      | NamedProp _ -> 0
      | CallProp _ -> 1
      | SpreadProp _ -> 2

    method tag_of_named_prop _env =
      function
      | Field _ -> 0
      | Method _ -> 1
      | Get _ -> 2
      | Set _ -> 3

    method tag_of_utility _ =
      function
      | Keys _ -> 0
      | Values _ -> 1
      | ReadOnly _ -> 2
      | Exact _ -> 3
      | Diff _ -> 4
      | Rest _ -> 5
      | ElementType _ -> 7
      | NonMaybeType _ -> 8
      | ObjMap _ -> 9
      | ObjMapi _ -> 10
      | TupleMap _ -> 11
      | Call _ -> 12
      | Class _ -> 13
      | Shape _ -> 14
      | ReactElementPropsType _ -> 18
      | ReactElementConfigType _ -> 19
      | ReactElementRefType _ -> 20
      | ReactConfigType _ -> 21
      | ObjKeyMirror _ -> 22
      | Partial _ -> 23
      | ObjMapConst _ -> 24

    method tag_of_polarity _ =
      function
      | Positive -> 0
      | Negative -> 1
      | Neutral -> 2

    method tag_of_bot_kind _env =
      function
      | EmptyType -> 0
      | EmptyMatchingPropT -> 1
      | EmptyTypeDestructorTriggerT _ -> 2
      | NoLowerWithUpper _ -> 3

    method tag_of_upper_bound_kind _env =
      function
      | NoUpper -> 0
      | SomeKnownUpper _ -> 1
      | SomeUnknownUpper _ -> 2

    method tag_of_builtin_or_symbol _ =
      function
      | FunProto -> 0
      | ObjProto -> 1
      | FunProtoApply -> 2
      | FunProtoBind -> 3
      | FunProtoCall -> 4
      | TSymbol _ -> 5
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

let mk_field_props prop_list =
  Base.List.map
    ~f:(fun (id, t, opt) ->
      NamedProp
        {
          name = id;
          prop = Field { t; polarity = Neutral; optional = opt };
          from_proto = false;
          def_loc = None;
        })
    prop_list

let mk_object ?(obj_kind = InexactObj) ?(obj_frozen = false) ?obj_literal obj_props =
  Obj { obj_kind; obj_frozen; obj_literal; obj_props }

let mk_generic_class symbol targs = Generic (symbol, ClassKind, targs)

let mk_generic_interface symbol targs = Generic (symbol, InterfaceKind, targs)

let mk_generic_talias symbol targs = Generic (symbol, TypeAliasKind, targs)

let rec mk_exact ty =
  match ty with
  | Obj o ->
    let obj_kind =
      match o.obj_kind with
      | InexactObj -> ExactObj
      | _ -> o.obj_kind
    in
    Obj { o with obj_kind }
  | Mu (i, t) -> Mu (i, mk_exact t)
  (* Not applicable *)
  | Any _
  | Top
  | Bot _
  | Void
  | Null
  | Symbol
  | Num _
  | Str _
  | Bool _
  | NumLit _
  | StrLit _
  | BoolLit _
  | Fun _
  | Arr _
  | Tup _
  | InlineInterface _
  | CharSet _ ->
    ty
  (* Do not nest $Exact *)
  | Utility (Exact _) -> ty
  (* Wrap in $Exact<...> *)
  | Generic _
  | TVar _
  | Bound _
  | Union _
  | Inter _
  | TypeOf _
  | Utility _
  | IndexedAccess _ ->
    Utility (Exact ty)

let mk_array ~readonly ~literal t =
  Arr { arr_readonly = readonly; arr_literal = literal; arr_elt_t = t }

let debug_string_of_generic_kind = function
  | ClassKind -> "class"
  | InterfaceKind -> "interface"
  | TypeAliasKind -> "type alias"
  | EnumKind -> "enum"

let string_of_utility_ctor = function
  | Keys _ -> "$Keys"
  | Values _ -> "$Values"
  | ReadOnly _ -> "$ReadOnly"
  | Partial _ -> "$Partial"
  | Exact _ -> "$Exact"
  | Diff _ -> "$Diff"
  | Rest _ -> "$Rest"
  | ElementType _ -> "$ElementType"
  | NonMaybeType _ -> "$NonMaybeType"
  | ObjMap _ -> "$ObjMap"
  | ObjMapi _ -> "$ObjMapi"
  | ObjKeyMirror _ -> "$KeyMirror"
  | ObjMapConst _ -> "$ObjectMapConst"
  | TupleMap _ -> "$TupleMap"
  | Call _ -> "$Call"
  | Class _ -> "Class"
  | Shape _ -> "$Shape"
  | ReactElementPropsType _ -> "React$ElementProps"
  | ReactElementConfigType _ -> "React$ElementConfig"
  | ReactElementRefType _ -> "React$ElementRef"
  | ReactConfigType _ -> "React$Config"

let types_of_utility = function
  | Keys t -> Some [t]
  | Values t -> Some [t]
  | ReadOnly t -> Some [t]
  | Partial t -> Some [t]
  | Exact t -> Some [t]
  | Diff (t1, t2) -> Some [t1; t2]
  | Rest (t1, t2) -> Some [t1; t2]
  | ElementType (t1, t2) -> Some [t1; t2]
  | NonMaybeType t -> Some [t]
  | ObjMap (t1, t2) -> Some [t1; t2]
  | ObjMapi (t1, t2) -> Some [t1; t2]
  | ObjKeyMirror t -> Some [t]
  | ObjMapConst (t1, t2) -> Some [t1; t2]
  | TupleMap (t1, t2) -> Some [t1; t2]
  | Call (t, ts) -> Some (t :: ts)
  | Class t -> Some [t]
  | Shape t -> Some [t]
  | ReactElementPropsType t -> Some [t]
  | ReactElementConfigType t -> Some [t]
  | ReactElementRefType t -> Some [t]
  | ReactConfigType (t1, t2) -> Some [t1; t2]
