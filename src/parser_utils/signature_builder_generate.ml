(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast
module LocMap = Loc_collections.LocMap
module Kind = Signature_builder_kind
module Entry = Signature_builder_entry
module Deps = Signature_builder_deps.With_Loc
module File_sig = File_sig.With_Loc
module Error = Deps.Error
module Dep = Deps.Dep

(* The generator creates new AST nodes, some of whose locations do not map back very accurately to
   original locations. While these are relatively unimportant, in that they should never make their
   way into type errors, making them Loc.none is risky because they would make Flow crash in the
   event of unforeseen bugs. Instead we reuse some nearby locations as approximations. *)
let approx_loc loc = loc

module T = struct
  type type_ = (Loc.t, Loc.t) Ast.Type.t

  and decl =
    (* type definitions *)
    | Type of {
        tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
        right: type_;
      }
    | OpaqueType of {
        tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
        impltype: type_ option;
        supertype: type_ option;
      }
    | Interface of {
        tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
        extends: generic list;
        body: Loc.t * object_type;
      }
    (* declarations and outlined expressions *)
    | ClassDecl of class_t
    | FunctionDecl of {
        annot: little_annotation;
        predicate: (Loc.t, Loc.t) Ast.Type.Predicate.t option;
      }
    | FunctionWithStaticsDecl of {
        base: Loc.t * expr_type;
        statics: ((Loc.t, Loc.t) Ast.Identifier.t * (Loc.t * expr_type)) list;
      }
    | VariableDecl of little_annotation
    (* remote *)
    | ImportNamed of {
        kind: Ast.Statement.ImportDeclaration.importKind;
        source: Loc.t Ast_utils.source;
        name: Loc.t Ast_utils.ident;
      }
    | ImportStar of {
        kind: Ast.Statement.ImportDeclaration.importKind;
        source: Loc.t Ast_utils.source;
      }
    | Require of {
        source: Loc.t Ast_utils.source;
        name: Loc.t Ast_utils.ident Nel.t option;
      }

  and generic = Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t

  and class_implement = (Loc.t, Loc.t) Ast.Class.Implements.t

  and little_annotation =
    | TYPE of type_
    | EXPR of (Loc.t * expr_type)

  and expr_type =
    (* types and expressions *)
    | Function of function_t
    | ObjectLiteral of {
        frozen: bool;
        properties: (Loc.t * object_property_t) Nel.t;
      }
    | ArrayLiteral of array_element_t Nel.t
    | ValueRef of reference (* typeof `x` *)
    | NumberLiteral of Ast.NumberLiteral.t
    | StringLiteral of Ast.StringLiteral.t
    | BooleanLiteral of bool
    | Number
    | String
    | Boolean
    | JSXLiteral of generic
    | Void
    | Null
    | Promise of (Loc.t * expr_type)
    | TypeCast of type_
    | Outline of outlinable_t
    | ObjectDestruct of little_annotation * (Loc.t * string)
    | FixMe

  and object_type = (Loc.t, Loc.t) Ast.Type.Object.t

  and object_key = (Loc.t, Loc.t) Ast.Expression.Object.Property.key

  and outlinable_t =
    | Class of (Loc.t * string) option * class_t
    | DynamicImport of Loc.t * Ast.StringLiteral.t
    | DynamicRequire of (Loc.t, Loc.t) Ast.Expression.t

  and function_t =
    | FUNCTION of {
        tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
        params: function_params;
        return: little_annotation;
      }

  and function_params = Loc.t * pattern list * (Loc.t * pattern) option

  and pattern = Loc.t * (Loc.t, Loc.t) Ast.Identifier.t option * bool (* optional *) * type_

  and class_t =
    | CLASS of {
        tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
        extends: generic option;
        implements: class_implement list;
        body: Loc.t * (Loc.t * class_element_t) list;
      }
    | DECLARE_CLASS of {
        tparams: (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
        extends: generic option;
        mixins: generic list;
        implements: class_implement list;
        body: Loc.t * object_type;
      }

  and class_element_t =
    | CMethod of object_key * Ast.Class.Method.kind * bool (* static *) * (Loc.t * function_t)
    | CProperty of object_key * bool (* static *) * Loc.t Ast.Variance.t option * type_
    | CPrivateField of string * bool (* static *) * Loc.t Ast.Variance.t option * type_

  and object_property_t =
    | OInit of object_key * (Loc.t * expr_type)
    | OMethod of object_key * (Loc.t * function_t)
    | OGet of object_key * (Loc.t * function_t)
    | OSet of object_key * (Loc.t * function_t)
    | OSpread of (Loc.t * expr_type)

  and array_element_t = AInit of (Loc.t * expr_type)

  and reference =
    | RLexical of Loc.t * string
    | RPath of Loc.t * reference * (Loc.t * string)

  module FixMe = struct
    let mk_type loc =
      ( loc,
        Ast.Type.Generic
          {
            Ast.Type.Generic.id =
              Ast.Type.Generic.Identifier.Unqualified
                (Flow_ast_utils.ident_of_source (loc, "$FlowFixMe"));
            targs = None;
          } )

    let mk_little_annotation loc = TYPE (mk_type loc)

    let mk_pattern default loc =
      if default then
        (loc, Some (Flow_ast_utils.ident_of_source (loc, "_")), true, mk_type loc)
      else
        (loc, None, false, mk_type loc)

    let mk_expr_type loc = (loc, FixMe)

    let mk_extends loc =
      Some
        ( loc,
          {
            Ast.Type.Generic.id =
              Ast.Type.Generic.Identifier.Unqualified
                (Flow_ast_utils.ident_of_source (loc, "$TEMPORARY$Super$FlowFixMe"));
            targs = None;
          } )

    let mk_decl loc = VariableDecl (mk_little_annotation loc)
  end

  let rec summarize_array loc = function
    | (AInit (_, et), aes) ->
      List.fold_left
        (fun acc -> function
          | AInit (_, et) -> data_optional_pair loc acc (Some et))
        (Some et)
        aes

  and data_optional_pair loc data1 data2 =
    match (data1, data2) with
    | (Some et1, Some et2) -> summarize_expr_type_pair loc et1 et2
    | (None, _)
    | (_, None) ->
      None

  and summarize_expr_type_pair loc expr_type1 expr_type2 =
    match (expr_type1, expr_type2) with
    | (ArrayLiteral array1, ArrayLiteral array2) ->
      let array' = summarize_array_pair loc array1 array2 in
      begin
        match array' with
        | None -> None
        | Some et -> Some (ArrayLiteral (AInit (loc, et), []))
      end
    | ( ObjectLiteral { frozen = frozen1; properties = object1 },
        ObjectLiteral { frozen = frozen2; properties = object2 } ) ->
      let frozen' =
        match (frozen1, frozen2) with
        | (true, true) -> Some true
        | (false, false) -> Some false
        | _ -> None
      in
      let object' = summarize_object_pair loc object1 object2 in
      begin
        match (frozen', object') with
        | (Some frozen, Some xets) ->
          Some
            (ObjectLiteral
               {
                 frozen;
                 properties = Nel.rev_map (fun (x, et) -> (loc, OInit (x, (loc, et)))) xets;
               })
        | _ -> None
      end
    | ((NumberLiteral _ | Number), (NumberLiteral _ | Number)) -> Some Number
    | ((StringLiteral _ | String), (StringLiteral _ | String)) -> Some String
    | ((BooleanLiteral _ | Boolean), (BooleanLiteral _ | Boolean)) -> Some Boolean
    | (Null, Null) -> Some Null
    | _ -> None

  and summarize_array_pair loc array1 array2 =
    data_optional_pair loc (summarize_array loc array1) (summarize_array loc array2)

  and summarize_object_pair =
    let abs_object_key object_key =
      Ast.Expression.Object.Property.(
        match object_key with
        | Literal (_, x) -> `Literal x
        | Identifier (_, x) -> `Identifier x
        | PrivateName (_, (_, x)) -> `PrivateName x
        | _ -> assert false)
    in
    let object_key loc abs_object_key =
      Ast.Expression.Object.Property.(
        match abs_object_key with
        | `Literal x -> Literal (loc, x)
        | `Identifier x -> Identifier (loc, x)
        | `PrivateName x -> PrivateName (loc, (loc, x)))
    in
    let compare_object_property =
      let abs_object_key = function
        | (_, OInit (object_key, _))
        | (_, OMethod (object_key, _))
        | (_, OGet (object_key, _))
        | (_, OSet (object_key, _)) ->
          abs_object_key object_key
        | (_, OSpread _) -> assert false
      in
      (fun op1 op2 -> Pervasives.compare (abs_object_key op1) (abs_object_key op2))
    in
    let summarize_object_property_pair loc op1 op2 =
      match (snd op1, snd op2) with
      | (OInit (object_key1, (_, et1)), OInit (object_key2, (_, et2))) ->
        let x = abs_object_key object_key1 in
        if x = abs_object_key object_key2 then
          match summarize_expr_type_pair loc et1 et2 with
          | Some et -> Some (object_key loc x, et)
          | None -> None
        else
          None
      | _ -> None
    in
    let rec summarize_object_pair loc acc = function
      | ([], []) -> acc
      | ([], _)
      | (_, []) ->
        None
      | (op1 :: ops1, op2 :: ops2) ->
        let acc =
          match (summarize_object_property_pair loc op1 op2, acc) with
          | (None, _)
          | (_, None) ->
            None
          | (Some xet, Some xets) -> Some (Nel.cons xet xets)
        in
        summarize_object_pair loc acc (ops1, ops2)
    in
    fun loc object1 object2 ->
      let (op1, ops1) =
        Nel.of_list_exn @@ List.sort compare_object_property @@ Nel.to_list object1
      in
      let (op2, ops2) =
        Nel.of_list_exn @@ List.sort compare_object_property @@ Nel.to_list object2
      in
      let init =
        match summarize_object_property_pair loc op1 op2 with
        | None -> None
        | Some xet -> Some (xet, [])
      in
      summarize_object_pair loc init (ops1, ops2)

  module Outlined : sig
    type 'a t

    val create : unit -> 'a t

    val next : 'a t -> Loc.t -> (Loc.t * string -> (Loc.t * string) option * 'a) -> Loc.t * string

    val get : 'a t -> 'a list
  end = struct
    type 'a t = (int * 'a list) ref

    let create () = ref (0, [])

    let next outlined outlined_loc f =
      let (n, l) = !outlined in
      let n = n + 1 in
      let id = (outlined_loc, Printf.sprintf "$%d" n) in
      let (id_opt, x) = f id in
      let (n, id) =
        match id_opt with
        | None -> (n, id)
        | Some id -> (n - 1, id)
      in
      let l = x :: l in
      outlined := (n, l);
      id

    let get outlined =
      let (_, l) = !outlined in
      l
  end

  let param_of_type (loc, name, optional, annot) =
    (loc, { Ast.Type.Function.Param.name; annot; optional })

  let type_of_generic (loc, gt) = (loc, Ast.Type.Generic gt)

  let source_of_source (loc, x) = (loc, { Ast.StringLiteral.value = x; raw = x })

  let temporary_type name loc t =
    ( loc,
      Ast.Type.Generic
        {
          Ast.Type.Generic.id =
            Ast.Type.Generic.Identifier.Unqualified (Flow_ast_utils.ident_of_source (loc, name));
          targs = Some (loc, [(loc, t)]);
        } )

  let rec type_of_expr_type outlined = function
    | (loc, Function function_t) -> type_of_function outlined (loc, function_t)
    | (loc, ObjectLiteral { frozen = true; properties = (pt, pts) }) ->
      temporary_type
        "$TEMPORARY$Object$freeze"
        loc
        (Ast.Type.Object
           {
             Ast.Type.Object.exact = true;
             inexact = false;
             properties = List.map (type_of_object_property outlined) (pt :: pts);
           })
    | (loc, ObjectLiteral { frozen = false; properties = (pt, pts) }) ->
      temporary_type
        "$TEMPORARY$object"
        loc
        (Ast.Type.Object
           {
             Ast.Type.Object.exact = true;
             inexact = false;
             properties = Base.List.map ~f:(type_of_object_property outlined) (pt :: pts);
           })
    | (loc, ArrayLiteral ets) ->
      temporary_type
        "$TEMPORARY$array"
        loc
        (match ets with
        | (et, []) -> snd (type_of_array_element outlined et)
        | (et1, et2 :: ets) ->
          Ast.Type.Union
            ( type_of_array_element outlined et1,
              type_of_array_element outlined et2,
              Base.List.map ~f:(type_of_array_element outlined) ets ))
    | (loc, ValueRef reference) ->
      ( loc,
        Ast.Type.Typeof
          (type_of_generic
             (loc, { Ast.Type.Generic.id = generic_id_of_reference reference; targs = None })) )
    | (loc, NumberLiteral nt) -> temporary_type "$TEMPORARY$number" loc (Ast.Type.NumberLiteral nt)
    | (loc, StringLiteral st) -> temporary_type "$TEMPORARY$string" loc (Ast.Type.StringLiteral st)
    | (loc, BooleanLiteral b) -> temporary_type "$TEMPORARY$boolean" loc (Ast.Type.BooleanLiteral b)
    | (loc, Number) -> (loc, Ast.Type.Number)
    | (loc, String) -> (loc, Ast.Type.String)
    | (loc, Boolean) -> (loc, Ast.Type.Boolean)
    | (loc, Void) -> (loc, Ast.Type.Void)
    | (loc, Promise t) ->
      ( loc,
        Ast.Type.Generic
          {
            Ast.Type.Generic.id =
              Ast.Type.Generic.Identifier.Unqualified
                (Flow_ast_utils.ident_of_source (loc, "Promise"));
            targs = Some (loc, [type_of_expr_type outlined t]);
          } )
    | (loc, Null) -> (loc, Ast.Type.Null)
    | (_loc, JSXLiteral g) -> type_of_generic g
    | (_loc, TypeCast t) -> t
    | (loc, Outline ht) ->
      let f = outlining_fun outlined loc ht in
      let id = Outlined.next outlined loc f in
      ( loc,
        Ast.Type.Typeof
          (type_of_generic
             ( loc,
               {
                 Ast.Type.Generic.id =
                   Ast.Type.Generic.Identifier.Unqualified (Flow_ast_utils.ident_of_source id);
                 targs = None;
               } )) )
    | (loc, ObjectDestruct (annot_or_init, prop)) ->
      let t = type_of_little_annotation outlined annot_or_init in
      let f id =
        ( None,
          ( fst t,
            Ast.Statement.DeclareVariable
              {
                Ast.Statement.DeclareVariable.id = Flow_ast_utils.ident_of_source id;
                annot = Ast.Type.Available (fst t, t);
              } ) )
      in
      let id = Outlined.next outlined loc f in
      ( loc,
        Ast.Type.Typeof
          (type_of_generic
             ( loc,
               {
                 Ast.Type.Generic.id =
                   Ast.Type.Generic.Identifier.Qualified
                     ( loc,
                       {
                         Ast.Type.Generic.Identifier.qualification =
                           Ast.Type.Generic.Identifier.Unqualified
                             (Flow_ast_utils.ident_of_source id);
                         id = Flow_ast_utils.ident_of_source prop;
                       } );
                 targs = None;
               } )) )
    | (loc, FixMe) -> FixMe.mk_type loc

  and generic_id_of_reference = function
    | RLexical (loc, x) ->
      Ast.Type.Generic.Identifier.Unqualified (Flow_ast_utils.ident_of_source (loc, x))
    | RPath (path_loc, reference, (loc, x)) ->
      Ast.Type.Generic.Identifier.Qualified
        ( path_loc,
          {
            Ast.Type.Generic.Identifier.qualification = generic_id_of_reference reference;
            id = Flow_ast_utils.ident_of_source (loc, x);
          } )

  and outlining_fun outlined decl_loc ht id =
    match ht with
    | Class (id_opt, class_t) ->
      ( id_opt,
        let id =
          match id_opt with
          | None -> id
          | Some id -> id
        in
        stmt_of_decl outlined decl_loc id (ClassDecl class_t) )
    | DynamicImport (source_loc, source_lit) ->
      ( None,
        let importKind = Ast.Statement.ImportDeclaration.ImportValue in
        let source = (source_loc, source_lit) in
        let default = None in
        let specifiers =
          Some
            (Ast.Statement.ImportDeclaration.ImportNamespaceSpecifier
               (decl_loc, Flow_ast_utils.ident_of_source id))
        in
        ( decl_loc,
          Ast.Statement.ImportDeclaration
            { Ast.Statement.ImportDeclaration.importKind; source; default; specifiers } ) )
    | DynamicRequire require ->
      ( None,
        let kind = Ast.Statement.VariableDeclaration.Const in
        let pattern =
          ( decl_loc,
            Ast.Pattern.Identifier
              {
                Ast.Pattern.Identifier.name = Flow_ast_utils.ident_of_source id;
                annot = Ast.Type.Missing (fst id);
                optional = false;
              } )
        in
        let declaration =
          { Ast.Statement.VariableDeclaration.Declarator.id = pattern; init = Some require }
        in
        ( decl_loc,
          Ast.Statement.VariableDeclaration
            { Ast.Statement.VariableDeclaration.kind; declarations = [(decl_loc, declaration)] } )
      )

  and type_of_array_element outlined = function
    | AInit expr_type -> type_of_expr_type outlined expr_type

  and type_of_object_property outlined = function
    | (loc, OInit (key, expr_type)) ->
      Ast.Type.Object.Property
        ( loc,
          {
            Ast.Type.Object.Property.key;
            value = Ast.Type.Object.Property.Init (type_of_expr_type outlined expr_type);
            optional = false;
            static = false;
            proto = false;
            _method = false;
            variance = None;
          } )
    | (loc, OMethod (key, function_t)) ->
      Ast.Type.Object.Property
        ( loc,
          {
            Ast.Type.Object.Property.key;
            value = Ast.Type.Object.Property.Init (type_of_function outlined function_t);
            optional = false;
            static = false;
            proto = false;
            _method = true;
            variance = None;
          } )
    | (loc, OGet (key, function_t)) ->
      Ast.Type.Object.Property
        ( loc,
          {
            Ast.Type.Object.Property.key;
            value = Ast.Type.Object.Property.Get (type_of_function_t outlined function_t);
            optional = false;
            static = false;
            proto = false;
            _method = false;
            variance = None;
          } )
    | (loc, OSet (key, function_t)) ->
      Ast.Type.Object.Property
        ( loc,
          {
            Ast.Type.Object.Property.key;
            value = Ast.Type.Object.Property.Set (type_of_function_t outlined function_t);
            optional = false;
            static = false;
            proto = false;
            _method = false;
            variance = None;
          } )
    | (loc, OSpread expr_type) ->
      Ast.Type.Object.SpreadProperty
        (loc, { Ast.Type.Object.SpreadProperty.argument = type_of_expr_type outlined expr_type })

  and type_of_function_t outlined = function
    | ( loc,
        FUNCTION
          {
            tparams : (Loc.t, Loc.t) Ast.Type.TypeParams.t option;
            params : function_params;
            return : little_annotation;
          } ) ->
      let (params_loc, params, rest) = params in
      ( loc,
        {
          Ast.Type.Function.tparams;
          params =
            ( params_loc,
              {
                Ast.Type.Function.Params.params = Base.List.map ~f:param_of_type params;
                rest =
                  (match rest with
                  | None -> None
                  | Some (loc, rest) ->
                    Some (loc, { Ast.Type.Function.RestParam.argument = param_of_type rest }));
              } );
          return = type_of_little_annotation outlined return;
        } )

  and type_of_function outlined function_t =
    let (loc, function_t) = type_of_function_t outlined function_t in
    (loc, Ast.Type.Function function_t)

  and type_of_little_annotation outlined = function
    | TYPE t -> t
    | EXPR expr_type -> type_of_expr_type outlined expr_type

  and annot_of_little_annotation outlined little_annotation =
    let t = type_of_little_annotation outlined little_annotation in
    (fst t, t)

  and name_opt_pattern id name_opt =
    let id_pattern =
      ( fst id,
        Ast.Pattern.Identifier
          { Ast.Pattern.Identifier.name = id; annot = Ast.Type.Missing (fst id); optional = false }
      )
    in
    match name_opt with
    | None -> id_pattern
    | Some (name, names) ->
      let (_, { Ast.Identifier.name = id_name; comments = _ }) = id in
      let pattern =
        ( fst name,
          Ast.Pattern.Object
            {
              Ast.Pattern.Object.properties =
                [
                  Ast.Pattern.Object.Property
                    ( fst name,
                      {
                        Ast.Pattern.Object.Property.key =
                          Ast.Pattern.Object.Property.Identifier
                            (Flow_ast_utils.ident_of_source name);
                        pattern = id_pattern;
                        shorthand = id_name = snd name;
                        default = None;
                      } );
                ];
              annot = Ast.Type.Missing (fst name);
            } )
      in
      wrap_name_pattern pattern names

  and wrap_name_pattern pattern = function
    | [] -> pattern
    | name :: names ->
      let pattern =
        ( fst name,
          Ast.Pattern.Object
            {
              Ast.Pattern.Object.properties =
                [
                  Ast.Pattern.Object.Property
                    ( fst name,
                      {
                        Ast.Pattern.Object.Property.key =
                          Ast.Pattern.Object.Property.Identifier
                            (Flow_ast_utils.ident_of_source name);
                        pattern;
                        shorthand = false;
                        default = None;
                      } );
                ];
              annot = Ast.Type.Missing (fst name);
            } )
      in
      wrap_name_pattern pattern names

  and stmt_of_decl outlined decl_loc id decl =
    let id = Flow_ast_utils.ident_of_source id in
    match decl with
    | Type { tparams; right } ->
      (decl_loc, Ast.Statement.TypeAlias { Ast.Statement.TypeAlias.id; tparams; right })
    | OpaqueType { tparams; impltype; supertype } ->
      ( decl_loc,
        Ast.Statement.OpaqueType { Ast.Statement.OpaqueType.id; tparams; impltype; supertype } )
    | Interface { tparams; extends; body } ->
      ( decl_loc,
        Ast.Statement.InterfaceDeclaration { Ast.Statement.Interface.id; tparams; extends; body } )
    | ClassDecl (CLASS { tparams; extends; implements; body = (body_loc, body) }) ->
      (* FIXME(T39206072, festevezga) Private properties are filtered to prevent an exception surfaced in https://github.com/facebook/flow/issues/7355 *)
      let filtered_body_FIXME =
        Base.List.filter
          ~f:(fun prop ->
            match prop with
            | (_loc, CPrivateField _) -> false
            | _ -> true)
          body
      in
      let body =
        ( body_loc,
          {
            Ast.Type.Object.exact = false;
            inexact = false;
            properties =
              Base.List.map ~f:(object_type_property_of_class_element outlined) filtered_body_FIXME;
          } )
      in
      let mixins = [] in
      ( decl_loc,
        Ast.Statement.DeclareClass
          { Ast.Statement.DeclareClass.id; tparams; extends; implements; mixins; body } )
    | ClassDecl (DECLARE_CLASS { tparams; extends; mixins; implements; body }) ->
      ( decl_loc,
        Ast.Statement.DeclareClass
          { Ast.Statement.DeclareClass.id; tparams; extends; implements; mixins; body } )
    | FunctionDecl { annot = little_annotation; predicate } ->
      ( decl_loc,
        Ast.Statement.DeclareFunction
          {
            Ast.Statement.DeclareFunction.id;
            annot = annot_of_little_annotation outlined little_annotation;
            predicate;
          } )
    | FunctionWithStaticsDecl { base; statics } ->
      let annot = type_of_expr_type outlined base in
      let properties =
        Base.List.rev_map
          ~f:(fun (id, expr) ->
            let annot = type_of_expr_type outlined expr in
            Ast.Type.Object.(
              Property
                ( fst id,
                  {
                    Property.key = Ast.Expression.Object.Property.Identifier id;
                    value = Property.Init annot;
                    optional = false;
                    static = false;
                    proto = false;
                    _method = false;
                    variance = None;
                  } )))
          statics
      in
      let ot = { Ast.Type.Object.exact = false; inexact = true; properties } in
      let assign = (decl_loc, Ast.Type.Object ot) in
      let t =
        let name = "$TEMPORARY$function" in
        let id =
          Ast.Type.Generic.Identifier.Unqualified (Flow_ast_utils.ident_of_source (decl_loc, name))
        in
        ( decl_loc,
          Ast.Type.Generic { Ast.Type.Generic.id; targs = Some (decl_loc, [annot; assign]) } )
      in
      ( decl_loc,
        Ast.Statement.DeclareVariable
          { Ast.Statement.DeclareVariable.id; annot = Ast.Type.Available (fst annot, t) } )
    | VariableDecl little_annotation ->
      ( decl_loc,
        Ast.Statement.DeclareVariable
          {
            Ast.Statement.DeclareVariable.id;
            annot = Ast.Type.Available (annot_of_little_annotation outlined little_annotation);
          } )
    | ImportNamed { kind; source; name } ->
      let importKind = kind in
      let source = source_of_source source in
      let default =
        if snd name = "default" then
          Some id
        else
          None
      in
      let specifiers =
        let (_, { Ast.Identifier.name = id_name; comments = _ }) = id in
        if snd name = "default" then
          None
        else
          Some
            (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers
               [
                 {
                   Ast.Statement.ImportDeclaration.kind = None;
                   local =
                     ( if id_name = snd name then
                       None
                     else
                       Some id );
                   remote = Flow_ast_utils.ident_of_source name;
                 };
               ])
      in
      ( decl_loc,
        Ast.Statement.ImportDeclaration
          { Ast.Statement.ImportDeclaration.importKind; source; default; specifiers } )
    | ImportStar { kind; source } ->
      let importKind = kind in
      let source = source_of_source source in
      let default = None in
      let specifiers =
        Some (Ast.Statement.ImportDeclaration.ImportNamespaceSpecifier (fst id, id))
      in
      ( decl_loc,
        Ast.Statement.ImportDeclaration
          { Ast.Statement.ImportDeclaration.importKind; source; default; specifiers } )
    | Require { source; name } ->
      let kind = Ast.Statement.VariableDeclaration.Const in
      let pattern = name_opt_pattern id name in
      let (loc, x) = source in
      let require =
        ( decl_loc,
          Ast.Expression.Call
            {
              Ast.Expression.Call.callee =
                ( approx_loc decl_loc,
                  Ast.Expression.Identifier
                    (Flow_ast_utils.ident_of_source (approx_loc decl_loc, "require")) );
              targs = None;
              arguments =
                [
                  Ast.Expression.Expression
                    ( loc,
                      Ast.Expression.Literal
                        {
                          Ast.Literal.value = Ast.Literal.String x;
                          raw = x;
                          comments = Flow_ast_utils.mk_comments_opt ();
                        } );
                ];
            } )
      in
      let declaration =
        { Ast.Statement.VariableDeclaration.Declarator.id = pattern; init = Some require }
      in
      ( decl_loc,
        Ast.Statement.VariableDeclaration
          { Ast.Statement.VariableDeclaration.kind; declarations = [(decl_loc, declaration)] } )

  and object_type_property_of_class_element outlined = function
    | (loc, CMethod (object_key, kind, static, f)) ->
      let (value, _method) =
        match kind with
        | Ast.Class.Method.Constructor
        | Ast.Class.Method.Method ->
          (Ast.Type.Object.Property.Init (type_of_function outlined f), true)
        | Ast.Class.Method.Get ->
          (Ast.Type.Object.Property.Get (type_of_function_t outlined f), false)
        | Ast.Class.Method.Set ->
          (Ast.Type.Object.Property.Set (type_of_function_t outlined f), false)
      in
      Ast.Type.Object.(
        Property
          ( loc,
            {
              Property.key = object_key;
              value;
              optional = false;
              static;
              proto = false;
              _method;
              variance = None;
            } ))
    | (loc, CProperty (object_key, static, variance, t)) ->
      Ast.Type.Object.(
        Property
          ( loc,
            {
              Property.key = object_key;
              value = Property.Init t;
              optional = false;
              static;
              proto = false;
              _method = false;
              variance;
            } ))
    | (_loc, CPrivateField (_x, _static, _variance, _t)) -> assert false
end

(* A signature of a module is described by exported expressions / definitions, but what we're really
   interested in is their types. In particular, we are interested in computing these types early, so
   that we can check the code inside a module against the signature in a separate pass. So the
   question is: what information is necessary to compute these types?

   Assuming we know how to map various kinds of type constructors (and destructors) to their
   meanings, all that remains to verify is that the types are well-formed: any identifiers appearing
   inside them should be defined in the top-level local scope, or imported, or global; and their
   "sort" of use (as a type or as a value) must match up with their definition.

   We break up the verification of well-formedness by computing a set of "dependencies" found by
   walking the structure of types, definitions, and expressions. The dependencies are simply the
   identifiers that are reached in this walk, coupled with their sort of use. Elsewhere, we
   recursively expand these dependencies by looking up the definitions of such identifiers, possibly
   uncovering further dependencies, and so on.

   A couple of important things to note at this point.

   1. The verification of well-formedness (and computation of types) is complete only up to the
   top-level local scope: any identifiers that are imported or global need to be resolved in a
   separate phase that builds things up in module-dependency order. To reflect this arrangement,
   verification returns not only a set of immediate errors but a set of conditions on imported and
   global identifiers that must be enforced by that separate phase.

   2. There is a fine line between errors found during verification and errors found during the
   computation of types (since both kinds of errors are static errors). Still, one might argue that
   the verification step should ensure that the computation step never fails. In that regard, the
   checks we have so far are not enough. In particular:

   (a) While classes are intended to be the only values that can be used as types, we also allow
   variables to be used as types, to account for the fact that a variable could be bound to a
   top-level local, imported, or global class. Ideally we would verify that these expectation is
   met, but we don't yet.

   (b) While destructuring only makes sense on types of the corresponding kinds (e.g., object
   destructuring would only work on object types), currently we allow destructuring on all
   types. Again, ideally we would discharge verification conditions for these and ensure that they
   are satisfied.

   (c) Parts of the module system are still under design. For example, can types be defined locally
   in anything other than the top-level scope? Do (or under what circumstances do) `require` and
   `import *` bring exported types in scope? These considerations will affect the computation step
   and ideally would be verified as well, but we're punting on them right now.
*)
module Eval (Env : Signature_builder_verify.EvalEnv) = struct
  let rec type_ t = t

  and type_params tparams = tparams

  and object_key key = key

  and object_type ot = ot

  and generic tr = tr

  and type_args = function
    | None -> None
    | Some (loc, ts) -> Some (loc, Base.List.map ~f:type_ ts)

  let rec annot_path = function
    | Kind.Annot_path.Annot (_, t) -> T.TYPE (type_ t)
    | Kind.Annot_path.Object (prop_loc, (path, (loc, x))) ->
      let annot = annot_path path in
      T.EXPR (prop_loc, T.ObjectDestruct (annot, (loc, x)))

  let rec init_path = function
    | Kind.Init_path.Init expr -> literal_expr expr
    | Kind.Init_path.Object (prop_loc, (path, (loc, x))) ->
      let expr_type = init_path path in
      ( prop_loc,
        (match expr_type with
        | (path_loc, T.ValueRef reference) -> T.ValueRef (T.RPath (path_loc, reference, (loc, x)))
        | _ -> T.ObjectDestruct (T.EXPR expr_type, (loc, x))) )

  and annotation loc ?init annot =
    match annot with
    | Some path -> annot_path path
    | None ->
      begin
        match init with
        | Some path -> T.EXPR (init_path path)
        | None -> T.FixMe.mk_little_annotation loc
      end

  and annotated_type = function
    | Ast.Type.Missing loc -> T.FixMe.mk_type loc
    | Ast.Type.Available (_, t) -> type_ t

  and pattern ?(default = false) patt =
    Ast.Pattern.(
      match patt with
      | (loc, Identifier { Identifier.annot; name; optional }) ->
        (loc, Some name, default || optional, annotated_type annot)
      | (loc, Object { Object.annot; properties = _ }) ->
        if default then
          (loc, Some (Flow_ast_utils.ident_of_source (loc, "_")), true, annotated_type annot)
        else
          (loc, None, false, annotated_type annot)
      | (loc, Array { Array.annot; elements = _; comments = _ }) ->
        if default then
          (loc, Some (Flow_ast_utils.ident_of_source (loc, "_")), true, annotated_type annot)
        else
          (loc, None, false, annotated_type annot)
      | (loc, Expression _) -> T.FixMe.mk_pattern default loc)

  and literal_expr =
    let string_value_of_object_key object_key =
      Ast.Expression.Object.Property.(
        match object_key with
        | Literal (loc, { Ast.Literal.value = Ast.Literal.String value; raw; comments = _ }) ->
          (loc, T.TypeCast (loc, Ast.Type.StringLiteral { Ast.StringLiteral.value; raw }))
        | Identifier (loc, { Ast.Identifier.name; comments = _ }) ->
          let value = name in
          let raw = Printf.sprintf "'%s'" name in
          (loc, T.TypeCast (loc, Ast.Type.StringLiteral { Ast.StringLiteral.value; raw }))
        | _ -> assert false)
    in
    let keys_as_string_values_of_object_properties object_properties =
      try
        Some
          (Nel.map
             (function
               | (loc, T.OInit (x, _)) -> (loc, T.OInit (x, string_value_of_object_key x))
               | _ -> assert false)
             object_properties)
      with _ -> None
    in
    Ast.Expression.(
      function
      | (loc, Literal { Ast.Literal.value; raw; comments = _ }) ->
        begin
          match value with
          | Ast.Literal.String value -> (loc, T.StringLiteral { Ast.StringLiteral.value; raw })
          | Ast.Literal.Number value -> (loc, T.NumberLiteral { Ast.NumberLiteral.value; raw })
          | Ast.Literal.Boolean b -> (loc, T.BooleanLiteral b)
          | Ast.Literal.Null -> (loc, T.Null)
          | _ -> T.FixMe.mk_expr_type loc
        end
      | (loc, TemplateLiteral _) -> (loc, T.String)
      | (loc, Identifier stuff) -> (loc, T.ValueRef (identifier stuff))
      | (loc, Class stuff) ->
        Ast.Class.(
          let { tparams; body; extends; implements; id; classDecorators = _; comments = _ } =
            stuff
          in
          let (super, super_targs) =
            match extends with
            | None -> (None, None)
            | Some (_, { Extends.expr; targs }) -> (Some expr, targs)
          in
          ( loc,
            T.Outline
              (T.Class
                 ( Option.map ~f:Flow_ast_utils.source_of_ident id,
                   class_ tparams body super super_targs implements )) ))
      | ( loc,
          Function
            {
              Ast.Function.generator;
              tparams;
              params;
              return;
              body;
              id = _;
              async;
              predicate = _;
              sig_loc = _;
            } ) ->
        (loc, T.Function (function_ generator async tparams params return body))
      | ( loc,
          ArrowFunction
            {
              Ast.Function.tparams;
              params;
              return;
              body;
              async;
              predicate = _;
              sig_loc = _;
              (* TODO: arrow functions can't have ids or be generators: *)
              id = _;
              generator = _;
            } ) ->
        (loc, T.Function (function_ false async tparams params return body))
      | (loc, Object stuff) ->
        Ast.Expression.Object.(
          let { properties; comments = _ } = stuff in
          begin
            match object_ properties with
            | Some o -> (loc, T.ObjectLiteral { frozen = false; properties = o })
            | None -> T.FixMe.mk_expr_type loc
          end)
      | (loc, Array stuff) ->
        Ast.Expression.Array.(
          let { elements; comments = _ } = stuff in
          begin
            match array_ elements with
            | Some a -> (loc, T.ArrayLiteral a)
            | None -> T.FixMe.mk_expr_type loc
          end)
      | (loc, TypeCast stuff) ->
        Ast.Expression.TypeCast.(
          let { annot; expression = _ } = stuff in
          let (_, t) = annot in
          (loc, T.TypeCast (type_ t)))
      | (loc, Member stuff) ->
        begin
          match member stuff with
          | Some ref_expr -> (loc, T.ValueRef ref_expr)
          | None -> T.FixMe.mk_expr_type loc
        end
      | ( loc,
          Import
            ( source_loc,
              ( Literal { Ast.Literal.value = Ast.Literal.String value; raw; comments = _ }
              | TemplateLiteral
                  {
                    TemplateLiteral.quasis =
                      [
                        ( _,
                          {
                            TemplateLiteral.Element.value =
                              { TemplateLiteral.Element.cooked = value; raw };
                            _;
                          } );
                      ];
                    _;
                  } ) ) ) ->
        (loc, T.Outline (T.DynamicImport (source_loc, { Ast.StringLiteral.value; raw })))
      | ( loc,
          Call
            {
              Ast.Expression.Call.callee =
                (_, Identifier (_, { Ast.Identifier.name = "require"; comments = _ }));
              _;
            } ) as expr ->
        (loc, T.Outline (T.DynamicRequire expr))
      | ( _,
          Call
            {
              Ast.Expression.Call.callee =
                ( _,
                  Member
                    {
                      Ast.Expression.Member._object =
                        (_, Identifier (_, { Ast.Identifier.name = "Object"; comments = _ }));
                      property =
                        Ast.Expression.Member.PropertyIdentifier
                          (_, { Ast.Identifier.name = "freeze"; comments = _ });
                    } );
              targs = None;
              arguments = [Expression (loc, Object stuff)];
            } ) ->
        Ast.Expression.Object.(
          let { properties; comments = _ } = stuff in
          begin
            match object_ properties with
            | Some o -> (loc, T.ObjectLiteral { frozen = true; properties = o })
            | None -> T.FixMe.mk_expr_type loc
          end)
      | ( _,
          Call
            {
              Ast.Expression.Call.callee =
                (_, Identifier (_, { Ast.Identifier.name = "keyMirror"; comments = _ }));
              targs = None;
              arguments = [Expression (loc, Object stuff)];
            } ) ->
        Ast.Expression.Object.(
          let { properties; comments = _ } = stuff in
          begin
            match object_ properties with
            | Some o ->
              begin
                match keys_as_string_values_of_object_properties o with
                | Some o' -> (loc, T.ObjectLiteral { frozen = false; properties = o' })
                | None -> T.FixMe.mk_expr_type loc
              end
            | None -> T.FixMe.mk_expr_type loc
          end)
      | (loc, Unary stuff) ->
        Ast.Expression.Unary.(
          let { operator; argument; comments = _ } = stuff in
          arith_unary operator loc argument)
      | (loc, Binary stuff) ->
        Ast.Expression.Binary.(
          let { operator; left; right } = stuff in
          arith_binary operator loc left right)
      | (loc, Sequence stuff) ->
        Ast.Expression.Sequence.(
          let { expressions } = stuff in
          begin
            match List.rev expressions with
            | expr :: _ -> literal_expr expr
            | [] -> T.FixMe.mk_expr_type loc
          end)
      | (loc, Assignment stuff) ->
        Ast.Expression.Assignment.(
          let { operator; left = _; right } = stuff in
          begin
            match operator with
            | None -> literal_expr right
            | Some _ -> T.FixMe.mk_expr_type loc
          end)
      | (loc, Update stuff) ->
        Ast.Expression.Update.(
          (* This operation has a simple result type. *)
          let { operator = _; argument = _; prefix = _ } = stuff in
          (loc, T.Number))
      | (loc, JSXElement e) ->
        Ast.JSX.(
          let { openingElement; closingElement = _; children = _ } = e in
          let (_loc, { Opening.name; selfClosing = _; attributes = _ }) = openingElement in
          begin
            match (name, Env.facebook_fbt) with
            | (Ast.JSX.Identifier (_loc_id, { Identifier.name = "fbt" }), Some custom_jsx_type) ->
              ( loc,
                T.JSXLiteral
                  ( loc,
                    {
                      Ast.Type.Generic.id =
                        Ast.Type.Generic.Identifier.Unqualified
                          (Flow_ast_utils.ident_of_source (loc, custom_jsx_type));
                      targs = None;
                    } ) )
            | _ -> T.FixMe.mk_expr_type loc
          end)
      | (loc, Call _)
      | (loc, Comprehension _)
      | (loc, Conditional _)
      | (loc, Generator _)
      | (loc, Import _)
      | (loc, JSXFragment _)
      | (loc, Logical _)
      | (loc, MetaProperty _)
      | (loc, New _)
      | (loc, OptionalCall _)
      | (loc, OptionalMember _)
      | (loc, Super)
      | (loc, TaggedTemplate _)
      | (loc, This)
      | (loc, Yield _) ->
        T.FixMe.mk_expr_type loc)

  and identifier stuff =
    let (loc, { Ast.Identifier.name; comments = _ }) = stuff in
    T.RLexical (loc, name)

  and member stuff =
    Ast.Expression.Member.(
      let { _object; property } = stuff in
      let ref_expr_opt = ref_expr _object in
      let name_opt =
        match property with
        | PropertyIdentifier (loc, x) -> Some (loc, x)
        | PropertyPrivateName (_, (loc, x)) -> Some (loc, x)
        | PropertyExpression _ -> None
      in
      match (ref_expr_opt, name_opt) with
      | (Some (path_loc, t), Some name) ->
        Some (T.RPath (path_loc, t, Flow_ast_utils.source_of_ident name))
      | (None, _)
      | (_, None) ->
        None)

  and ref_expr expr =
    Ast.Expression.(
      match expr with
      | (loc, Identifier stuff) -> Some (loc, identifier stuff)
      | (loc, Member stuff) ->
        begin
          match member stuff with
          | Some ref_expr -> Some (loc, ref_expr)
          | None -> None
        end
      | _ -> None)

  and arith_unary operator loc argument =
    Ast.Expression.Unary.(
      match operator with
      (* These operations have simple result types. *)
      | Plus -> (loc, T.Number)
      | BitNot -> (loc, T.Number)
      | Typeof -> (loc, T.String)
      | Void -> (loc, T.Void)
      | Delete -> (loc, T.Boolean)
      (* These operations may or may not have simple result types. See associated TODO: comment in
         Signature_builder_verify. *)
      | Minus ->
        begin
          match literal_expr argument with
          | (_, T.NumberLiteral { Ast.NumberLiteral.value; raw }) ->
            (loc, T.NumberLiteral { Ast.NumberLiteral.value = -.value; raw = "-" ^ raw })
          | _ -> (loc, T.Number)
        end
      | Not ->
        begin
          match literal_expr argument with
          | (_, T.BooleanLiteral b) -> (loc, T.BooleanLiteral (not b))
          | (_, T.Function _)
          | (_, T.ObjectLiteral _)
          | (_, T.ArrayLiteral _)
          | (_, T.JSXLiteral _) ->
            (loc, T.BooleanLiteral false)
          | (_, T.Void)
          | (_, T.Null) ->
            (loc, T.BooleanLiteral true)
          | (_, T.NumberLiteral { Ast.NumberLiteral.value; _ }) ->
            (loc, T.BooleanLiteral (value = 0.))
          | (_, T.StringLiteral { Ast.StringLiteral.value; _ }) ->
            (loc, T.BooleanLiteral (value = ""))
          | _ -> (loc, T.Boolean)
        end
      | Await ->
        (* The result type of this operation depends in a complicated way on the argument type. *)
        T.FixMe.mk_expr_type loc)

  and arith_binary operator loc _left _right =
    Ast.Expression.Binary.(
      match operator with
      | Plus ->
        (* The result type of this operation depends in a complicated way on the argument type. *)
        T.FixMe.mk_expr_type loc
      (* These operations have simple result types. *)
      | Equal -> (loc, T.Boolean)
      | NotEqual -> (loc, T.Boolean)
      | StrictEqual -> (loc, T.Boolean)
      | StrictNotEqual -> (loc, T.Boolean)
      | LessThan -> (loc, T.Boolean)
      | LessThanEqual -> (loc, T.Boolean)
      | GreaterThan -> (loc, T.Boolean)
      | GreaterThanEqual -> (loc, T.Boolean)
      | LShift -> (loc, T.Number)
      | RShift -> (loc, T.Number)
      | RShift3 -> (loc, T.Number)
      | Minus -> (loc, T.Number)
      | Mult -> (loc, T.Number)
      | Exp -> (loc, T.Number)
      | Div -> (loc, T.Number)
      | Mod -> (loc, T.Number)
      | BitOr -> (loc, T.Number)
      | Xor -> (loc, T.Number)
      | BitAnd -> (loc, T.Number)
      | In -> (loc, T.Boolean)
      | Instanceof -> (loc, T.Boolean))

  and function_param (_, { Ast.Function.Param.argument; default }) =
    pattern ~default:(default <> None) argument

  and function_rest_param (loc, { Ast.Function.RestParam.argument }) = (loc, pattern argument)

  and function_params params =
    Ast.Function.(
      let (params_loc, { Params.params; rest }) = params in
      let params = Base.List.map ~f:function_param params in
      let rest =
        match rest with
        | None -> None
        | Some param -> Some (function_rest_param param)
      in
      (params_loc, params, rest))

  and function_return ~is_missing_ok ~async return =
    match return with
    | Ast.Type.Missing loc ->
      if is_missing_ok () then
        let t = T.Void in
        let t =
          if async then
            T.Promise (loc, t)
          else
            t
        in
        T.EXPR (loc, t)
      else
        T.FixMe.mk_little_annotation loc
    | Ast.Type.Available (_, t) -> T.TYPE (type_ t)

  and function_predicate body predicate =
    match (predicate, body) with
    | (None, _) -> None
    | ( Some (loc, Ast.Type.Predicate.Inferred),
        ( Ast.Function.BodyBlock
            ( _,
              {
                Ast.Statement.Block.body =
                  [(_, Ast.Statement.Return { Ast.Statement.Return.argument = Some e; _ })];
              } )
        | Ast.Function.BodyExpression e ) ) ->
      Some (loc, Ast.Type.Predicate.Declared e)
    | (Some (_, Ast.Type.Predicate.Inferred), _) -> None
    | (Some (_, Ast.Type.Predicate.Declared _), _) -> predicate

  and function_ generator async tparams params return body =
    let tparams = type_params tparams in
    let params = function_params params in
    let return =
      let is_missing_ok () = (not generator) && Signature_utils.Procedure_decider.is body in
      function_return ~is_missing_ok ~async return
    in
    (* TODO: It is unclear what happens for generator functions. In particular,
       what do declarations of such functions look like, aside from the return type being
       `Generator<...>`? *)
    T.FUNCTION { tparams; params; return }

  and class_ =
    let class_element acc element =
      Ast.Class.(
        match element with
        | Body.Method
            ( _,
              {
                Method.key =
                  Ast.Expression.Object.Property.Identifier
                    (_, { Ast.Identifier.name; comments = _ });
                _;
              } )
        | Body.Property
            ( _,
              {
                Property.key =
                  Ast.Expression.Object.Property.Identifier
                    (_, { Ast.Identifier.name; comments = _ });
                _;
              } )
          when (not Env.prevent_munge) && Signature_utils.is_munged_property_name name ->
          acc
        | Body.Property
            ( _,
              {
                Property.key =
                  Ast.Expression.Object.Property.Identifier
                    (_, { Ast.Identifier.name = "propTypes"; comments = _ });
                static = true;
                _;
              } )
          when Env.ignore_static_propTypes ->
          acc
        | Body.Method (elem_loc, { Method.key; value; kind; static; decorators = _ }) ->
          let x = object_key key in
          let ( loc,
                {
                  Ast.Function.generator;
                  tparams;
                  params;
                  return;
                  body;
                  id = _;
                  async;
                  predicate = _;
                  sig_loc = _;
                } ) =
            value
          in
          ( elem_loc,
            T.CMethod (x, kind, static, (loc, function_ generator async tparams params return body))
          )
          :: acc
        | Body.Property (elem_loc, { Property.key; annot; static; variance; value = _ }) ->
          let x = object_key key in
          (elem_loc, T.CProperty (x, static, variance, annotated_type annot)) :: acc
        | Body.PrivateField
            ( elem_loc,
              {
                PrivateField.key = (_, (_, { Ast.Identifier.name = x; comments = _ }));
                annot;
                static;
                variance;
                value = _;
              } ) ->
          (elem_loc, T.CPrivateField (x, static, variance, annotated_type annot)) :: acc)
    in
    fun tparams body super super_targs implements ->
      Ast.Class.(
        let (body_loc, { Body.body }) = body in
        let tparams = type_params tparams in
        let body = List.rev @@ List.fold_left class_element [] body in
        let extends =
          match super with
          | None -> None
          | Some expr ->
            let ref_expr_opt = ref_expr expr in
            begin
              match ref_expr_opt with
              | Some (loc, reference) ->
                Some
                  ( loc,
                    {
                      Ast.Type.Generic.id = T.generic_id_of_reference reference;
                      targs = type_args super_targs;
                    } )
              | None -> T.FixMe.mk_extends (fst expr)
            end
        in
        let implements = Base.List.map ~f:class_implement implements in
        T.CLASS { tparams; extends; implements; body = (body_loc, body) })

  and array_ =
    let array_element expr_or_spread_opt =
      Ast.Expression.(
        match expr_or_spread_opt with
        | None -> assert false
        | Some (Expression expr) -> T.AInit (literal_expr expr)
        | Some (Spread _spread) -> assert false)
    in
    function
    | [] -> None
    | t :: ts -> (try Some (Nel.map array_element (t, ts)) with _ -> None)

  and class_implement implement = implement

  and object_ =
    let object_property =
      Ast.Expression.Object.Property.(
        function
        | (loc, Init { key; value; shorthand = _ }) ->
          let x = object_key key in
          (loc, T.OInit (x, literal_expr value))
        | (loc, Method { key; value = (fn_loc, fn) }) ->
          let x = object_key key in
          let {
            Ast.Function.generator;
            tparams;
            params;
            return;
            body;
            id = _;
            async;
            predicate = _;
            sig_loc = _;
          } =
            fn
          in
          (loc, T.OMethod (x, (fn_loc, function_ generator async tparams params return body)))
        | (loc, Get { key; value = (fn_loc, fn) }) ->
          let x = object_key key in
          let {
            Ast.Function.generator;
            tparams;
            params;
            return;
            body;
            id = _;
            async;
            predicate = _;
            sig_loc = _;
          } =
            fn
          in
          (loc, T.OGet (x, (fn_loc, function_ generator async tparams params return body)))
        | (loc, Set { key; value = (fn_loc, fn) }) ->
          let x = object_key key in
          let {
            Ast.Function.generator;
            tparams;
            params;
            return;
            body;
            id = _;
            async;
            predicate = _;
            sig_loc = _;
          } =
            fn
          in
          (loc, T.OSet (x, (fn_loc, function_ generator async tparams params return body))))
    in
    let object_spread_property =
      Ast.Expression.Object.SpreadProperty.(
        (fun (loc, { argument }) -> (loc, T.OSpread (literal_expr argument))))
    in
    function
    | [] -> None
    | property :: properties ->
      Ast.Expression.Object.(
        (try
           Some
             (Nel.map
                (function
                  | Property p -> object_property p
                  | SpreadProperty p -> object_spread_property p)
                (property, properties))
         with _ -> None))
end

module Generator (Env : Signature_builder_verify.EvalEnv) = struct
  module Eval = Eval (Env)

  let rec eval (loc, kind) =
    match kind with
    | Kind.WithPropertiesDef { base; properties } ->
      begin
        match Kind.get_function_kind_info base with
        | Some (generator, async, tparams, params, return, body) ->
          T.FunctionWithStaticsDecl
            {
              base = (loc, T.Function (Eval.function_ generator async tparams params return body));
              statics =
                Base.List.map properties ~f:(fun (id_prop, expr) ->
                    (id_prop, Eval.literal_expr expr));
            }
        | None -> eval (loc, base)
      end
    | Kind.VariableDef { id = _; annot; init } -> T.VariableDecl (Eval.annotation loc ?init annot)
    | Kind.FunctionDef { generator; async; tparams; params; return; body; predicate } ->
      let annot =
        T.EXPR (loc, T.Function (Eval.function_ generator async tparams params return body))
      in
      let predicate = Eval.function_predicate body predicate in
      T.FunctionDecl { annot; predicate }
    | Kind.DeclareFunctionDef { annot = (_, t); predicate } ->
      T.FunctionDecl { annot = T.TYPE (Eval.type_ t); predicate }
    | Kind.ClassDef { tparams; body; super; super_targs; implements } ->
      T.ClassDecl (Eval.class_ tparams body super super_targs implements)
    | Kind.DeclareClassDef { tparams; body = (body_loc, body); extends; mixins; implements } ->
      let tparams = Eval.type_params tparams in
      let body = Eval.object_type body in
      let extends =
        match extends with
        | None -> None
        | Some r -> Some (Eval.generic r)
      in
      let mixins = Base.List.map ~f:Eval.generic mixins in
      let implements = Base.List.map ~f:Eval.class_implement implements in
      T.ClassDecl
        (T.DECLARE_CLASS { tparams; extends; mixins; implements; body = (body_loc, body) })
    | Kind.TypeDef { tparams; right } ->
      let tparams = Eval.type_params tparams in
      let right = Eval.type_ right in
      T.Type { tparams; right }
    | Kind.OpaqueTypeDef { tparams; impltype; supertype } ->
      let tparams = Eval.type_params tparams in
      let impltype =
        match impltype with
        | None -> None
        | Some t -> Some (Eval.type_ t)
      in
      let supertype =
        match supertype with
        | None -> None
        | Some t -> Some (Eval.type_ t)
      in
      T.OpaqueType { tparams; impltype; supertype }
    | Kind.InterfaceDef { tparams; extends; body = (body_loc, body) } ->
      let tparams = Eval.type_params tparams in
      let extends = Base.List.map ~f:Eval.generic extends in
      let body = Eval.object_type body in
      T.Interface { tparams; extends; body = (body_loc, body) }
    | Kind.ImportNamedDef { kind; source; name } -> T.ImportNamed { kind; source; name }
    | Kind.ImportStarDef { kind; source } -> T.ImportStar { kind; source }
    | Kind.RequireDef { source; name } -> T.Require { source; name }
    | Kind.SketchyToplevelDef -> T.FixMe.mk_decl loc

  let make_env_entry outlined n entries acc =
    let entries = Loc_collections.LocMap.bindings entries in
    let (acc, _) =
      List.fold_left
        (fun (acc_stmt, acc_ctor) (loc, kind) ->
          let ctor = Signature_builder_kind.kind_to_ctor (snd kind) in
          let (add_entry, acc_ctor) =
            match acc_ctor with
            | None ->
              (* Include the first occurrence, and set the kind of the entries *)
              (true, Some ctor)
            | Some Signature_builder_kind.DeclareFunctionDefKind
              when ctor = Signature_builder_kind.DeclareFunctionDefKind ->
              (* Multiple function declarations correspond to function overloads.
               * Only allow these if this is a "function declaration" kind. *)
              (true, acc_ctor)
            | Some _ ->
              (* Ignore any other entry *)
              (false, acc_ctor)
          in
          let acc_stmt =
            if add_entry then
              let id = (loc, n) in
              let dt = eval kind in
              let decl_loc = fst kind in
              T.stmt_of_decl outlined decl_loc id dt :: acc_stmt
            else
              acc_stmt
          in
          (acc_stmt, acc_ctor))
        (acc, None)
        entries
    in
    acc

  let make_env outlined env = SMap.fold (make_env_entry outlined) env []

  let cjs_exports =
    let declare_module_exports mod_exp_loc loc t =
      (mod_exp_loc, Ast.Statement.DeclareModuleExports (loc, t))
    in
    let additional_properties_of_module_exports outlined add_module_exports_list =
      Base.List.rev_map
        ~f:(fun (id, expr) ->
          let annot = T.type_of_expr_type outlined (Eval.literal_expr expr) in
          Ast.Type.Object.(
            Property
              ( fst id,
                {
                  Property.key =
                    Ast.Expression.Object.Property.Identifier (Flow_ast_utils.ident_of_source id);
                  value = Property.Init annot;
                  optional = false;
                  static = false;
                  proto = false;
                  _method = false;
                  variance = None;
                } )))
        add_module_exports_list
    in
    let set_module_exports mod_exp_loc outlined expr add_module_exports_list =
      let annot = T.type_of_expr_type outlined (Eval.literal_expr expr) in
      if ListUtils.is_empty add_module_exports_list then
        (mod_exp_loc, Ast.Statement.DeclareModuleExports (fst annot, annot))
      else
        let properties = additional_properties_of_module_exports outlined add_module_exports_list in
        let ot = { Ast.Type.Object.exact = false; inexact = true; properties } in
        let assign = (mod_exp_loc, Ast.Type.Object ot) in
        let t =
          let name = "$TEMPORARY$module$exports$assign" in
          let id =
            Ast.Type.Generic.Identifier.Unqualified
              (Flow_ast_utils.ident_of_source (mod_exp_loc, name))
          in
          ( mod_exp_loc,
            Ast.Type.Generic { Ast.Type.Generic.id; targs = Some (mod_exp_loc, [annot; assign]) } )
        in
        (mod_exp_loc, Ast.Statement.DeclareModuleExports (fst annot, t))
    in
    let add_module_exports mod_exp_loc outlined add_module_exports_list =
      let properties = additional_properties_of_module_exports outlined add_module_exports_list in
      let ot = { Ast.Type.Object.exact = true; inexact = false; properties } in
      let t = (mod_exp_loc, Ast.Type.Object ot) in
      [(mod_exp_loc, Ast.Statement.DeclareModuleExports (mod_exp_loc, t))]
    in
    fun outlined -> function
      | (None, _) -> []
      | (Some mod_exp_loc, list) ->
        let (declare_module_exports_list, set_module_exports_list, add_module_exports_list) =
          List.fold_left
            (fun (declare_module_exports_list, set_module_exports_list, add_module_exports_list) ->
               function
              | File_sig.DeclareModuleExportsDef (loc, t) ->
                ((loc, t) :: declare_module_exports_list, [], [])
              | File_sig.SetModuleExportsDef expr ->
                ( declare_module_exports_list,
                  (expr, add_module_exports_list) :: set_module_exports_list,
                  [] )
              | File_sig.AddModuleExportsDef (id, expr) ->
                ( declare_module_exports_list,
                  set_module_exports_list,
                  (id, expr) :: add_module_exports_list ))
            ([], [], [])
            list
        in
        (match (declare_module_exports_list, set_module_exports_list, add_module_exports_list) with
        | (_ :: _, _, _) ->
          (* if there are any `declare module.exports: ...`, then the last such wins *)
          let (loc, t) = List.hd (List.rev declare_module_exports_list) in
          [declare_module_exports mod_exp_loc loc t]
        | ([], _ :: _, _) ->
          (* if there are any `module.exports = ...`, then the last such wins *)
          let (expr, add_module_exports_list) = List.hd (List.rev set_module_exports_list) in
          [set_module_exports mod_exp_loc outlined expr add_module_exports_list]
        | ([], [], _) ->
          (* otherwise, collect every `module.exports.X = ...` *)
          add_module_exports mod_exp_loc outlined add_module_exports_list)

  let eval_export_default_declaration =
    Ast.Statement.ExportDefaultDeclaration.(
      function
      | Declaration
          ( loc,
            Ast.Statement.FunctionDeclaration
              ({ Ast.Function.id = Some _; _ } as function_declaration) ) ->
        `Decl (Entry.function_declaration loc function_declaration)
      | Declaration
          ( loc,
            Ast.Statement.FunctionDeclaration
              {
                Ast.Function.id = None;
                generator;
                tparams;
                params;
                return;
                body;
                async;
                predicate = _;
                sig_loc = _;
              } ) ->
        `Expr (loc, T.Function (Eval.function_ generator async tparams params return body))
      | Declaration (loc, Ast.Statement.ClassDeclaration ({ Ast.Class.id = Some _; _ } as class_))
        ->
        `Decl (Entry.class_ loc class_)
      | Declaration
          ( loc,
            Ast.Statement.ClassDeclaration
              {
                Ast.Class.id = None;
                tparams;
                body;
                extends;
                implements;
                classDecorators = _;
                comments = _;
              } ) ->
        let (super, super_targs) =
          match extends with
          | None -> (None, None)
          | Some (_, { Ast.Class.Extends.expr; targs }) -> (Some expr, targs)
        in
        `Expr
          (loc, T.Outline (T.Class (None, Eval.class_ tparams body super super_targs implements)))
      | Declaration _stmt -> assert false
      | Expression (loc, Ast.Expression.Function ({ Ast.Function.id = Some _; _ } as function_)) ->
        `Decl (Entry.function_declaration loc function_)
      | Expression expr -> `Expr (Eval.literal_expr expr))

  let export_name export_loc ?exported ?source local exportKind =
    ( export_loc,
      Ast.Statement.ExportNamedDeclaration
        {
          Ast.Statement.ExportNamedDeclaration.declaration = None;
          specifiers =
            Some
              (Ast.Statement.ExportNamedDeclaration.ExportSpecifiers
                 [
                   ( approx_loc export_loc,
                     {
                       Ast.Statement.ExportNamedDeclaration.ExportSpecifier.local =
                         Flow_ast_utils.ident_of_source local;
                       exported = Option.map ~f:Flow_ast_utils.ident_of_source exported;
                     } );
                 ]);
          source;
          exportKind;
        } )

  let export_named_specifier export_loc local remote source exportKind =
    let exported =
      if snd remote = snd local then
        None
      else
        Some remote
    in
    let source =
      match source with
      | None -> None
      | Some source -> Some (T.source_of_source source)
    in
    export_name export_loc ?exported ?source local exportKind

  let export_star export_loc star_loc ?remote source exportKind =
    ( export_loc,
      Ast.Statement.ExportNamedDeclaration
        {
          Ast.Statement.ExportNamedDeclaration.declaration = None;
          specifiers =
            Some
              (Ast.Statement.ExportNamedDeclaration.ExportBatchSpecifier
                 (star_loc, Option.map ~f:Flow_ast_utils.ident_of_source remote));
          source = Some (T.source_of_source source);
          exportKind;
        } )

  let declare_export_default_declaration export_loc default_loc declaration =
    ( export_loc,
      Ast.Statement.DeclareExportDeclaration
        {
          default = Some default_loc;
          Ast.Statement.DeclareExportDeclaration.declaration = Some declaration;
          specifiers = None;
          source = None;
        } )

  let export_value_named_declaration export_loc local =
    export_name export_loc local Ast.Statement.ExportValue

  let export_value_default_named_declaration export_loc default local =
    export_name export_loc local ~exported:default Ast.Statement.ExportValue

  let export_value_named_specifier export_loc local remote source =
    export_named_specifier export_loc local remote source Ast.Statement.ExportValue

  let export_value_star export_loc star_loc source =
    export_star export_loc star_loc source Ast.Statement.ExportValue

  let export_value_ns_star export_loc star_loc ns source =
    export_star export_loc star_loc ~remote:ns source Ast.Statement.ExportValue

  let export_type_named_declaration export_loc local =
    export_name export_loc local Ast.Statement.ExportType

  let export_type_named_specifier export_loc local remote source =
    export_named_specifier export_loc local remote source Ast.Statement.ExportType

  let export_type_star export_loc star_loc source =
    export_star export_loc star_loc source Ast.Statement.ExportType

  let eval_export_value_bindings outlined named named_infos star =
    File_sig.(
      let (named, ns) =
        List.partition
          (function
            | (_, (_, ExportNamed { kind = NamedSpecifier _; _ }))
            | (_, (_, ExportNs _)) ->
              false
            | (_, (_, _)) -> true)
          named
      in
      let stmts =
        List.fold_left
          (fun acc -> function
            | (export_loc, ExportStar { star_loc; source }) ->
              export_value_star export_loc star_loc source :: acc)
          []
          star
      in
      let seen = ref SSet.empty in
      let stmts =
        List.fold_left2
          (fun acc (n, (export_loc, export)) export_def ->
            if SSet.mem n !seen then
              acc
            else (
              seen := SSet.add n !seen;
              match (export, export_def) with
              | (ExportDefault { default_loc; local }, DeclareExportDef decl) ->
                begin
                  match local with
                  | Some id ->
                    export_value_default_named_declaration export_loc (default_loc, n) id :: acc
                  | None -> declare_export_default_declaration export_loc default_loc decl :: acc
                end
              | (ExportDefault { default_loc; _ }, ExportDefaultDef decl) ->
                begin
                  match eval_export_default_declaration decl with
                  | `Decl (id, _kind) ->
                    export_value_default_named_declaration
                      export_loc
                      (default_loc, n)
                      (Flow_ast_utils.source_of_ident id)
                    :: acc
                  | `Expr expr_type ->
                    let declaration =
                      Ast.Statement.DeclareExportDeclaration.DefaultType
                        (T.type_of_expr_type outlined expr_type)
                    in
                    declare_export_default_declaration export_loc default_loc declaration :: acc
                end
              | (ExportNamed { loc; kind = NamedDeclaration }, DeclareExportDef _decl) ->
                export_value_named_declaration export_loc (loc, n) :: acc
              | (ExportNamed { loc; kind = NamedDeclaration }, ExportNamedDef _stmt) ->
                export_value_named_declaration export_loc (loc, n) :: acc
              | _ -> assert false
            ))
          stmts
          named
          named_infos
      in
      List.fold_left
        (fun acc (n, (export_loc, export)) ->
          match export with
          | ExportNamed { loc; kind = NamedSpecifier { local = name; source } } ->
            export_value_named_specifier export_loc name (loc, n) source :: acc
          | ExportNs { loc; star_loc; source } ->
            export_value_ns_star export_loc star_loc (loc, n) source :: acc
          | _ -> assert false)
        stmts
        ns)

  let eval_export_type_bindings type_named type_named_infos type_star =
    File_sig.(
      let (type_named, type_ns) =
        List.partition
          (function
            | (_, (_, TypeExportNamed { kind = NamedSpecifier _; _ })) -> false
            | (_, (_, _)) -> true)
          type_named
      in
      let stmts =
        List.fold_left
          (fun acc -> function
            | (export_loc, ExportStar { star_loc; source }) ->
              export_type_star export_loc star_loc source :: acc)
          []
          type_star
      in
      let stmts =
        List.fold_left2
          (fun acc (n, (export_loc, export)) export_def ->
            (match (export, export_def) with
            | (TypeExportNamed { loc; kind = NamedDeclaration }, DeclareExportDef _decl) ->
              export_type_named_declaration export_loc (loc, n)
            | (TypeExportNamed { loc; kind = NamedDeclaration }, ExportNamedDef _stmt) ->
              export_type_named_declaration export_loc (loc, n)
            | _ -> assert false)
            :: acc)
          stmts
          type_named
          type_named_infos
      in
      List.fold_left
        (fun acc (n, (export_loc, export)) ->
          (match export with
          | TypeExportNamed { loc; kind = NamedSpecifier { local = name; source } } ->
            export_type_named_specifier export_loc name (loc, n) source
          | _ -> assert false)
          :: acc)
        stmts
        type_ns)

  let exports outlined file_sig =
    File_sig.(
      let module_sig = file_sig.module_sig in
      let { info = exports_info; module_kind; type_exports_named; type_exports_star; requires = _ }
          =
        module_sig
      in
      let { module_kind_info; type_exports_named_info } = exports_info in
      let values =
        match (module_kind, module_kind_info) with
        | (CommonJS { mod_exp_loc }, CommonJSInfo cjs_exports_defs) ->
          cjs_exports outlined (mod_exp_loc, cjs_exports_defs)
        | (ES { named; star }, ESInfo named_infos) ->
          eval_export_value_bindings outlined named named_infos star
        | _ -> assert false
      in
      let types =
        eval_export_type_bindings type_exports_named type_exports_named_info type_exports_star
      in
      (values, types))

  let relativize loc program_loc =
    Loc.
      {
        program_loc with
        start = { line = program_loc._end.line + loc.start.line; column = loc.start.column };
        _end = { line = program_loc._end.line + loc._end.line; column = loc._end.column };
      }

  let make env file_sig program =
    let (program_loc, _, _) = program in
    let outlined = T.Outlined.create () in
    let env = make_env outlined env in
    let (values, types) = exports outlined file_sig in
    let outlined_stmts = T.Outlined.get outlined in
    ( program_loc,
      List.sort Pervasives.compare (List.rev_append env @@ List.rev outlined_stmts)
      @ List.sort Pervasives.compare (List.rev_append values @@ List.rev types),
      [] )

  (* no need to include the comments *)
end
