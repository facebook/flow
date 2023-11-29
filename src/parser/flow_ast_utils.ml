(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast

type 'loc binding = 'loc * string

type 'loc ident = 'loc * string [@@deriving show]

type 'loc source = 'loc * string [@@deriving show]

let rec fold_bindings_of_pattern =
  Pattern.(
    let property f acc =
      Object.(
        function
        | Property (_, { Property.pattern = p; _ })
        | RestElement (_, { RestElement.argument = p; comments = _ }) ->
          fold_bindings_of_pattern f acc p
      )
    in
    let element f acc =
      Array.(
        function
        | Hole _ -> acc
        | Element (_, { Element.argument = p; default = _ })
        | RestElement (_, { RestElement.argument = p; comments = _ }) ->
          fold_bindings_of_pattern f acc p
      )
    in
    fun f acc -> function
      | (_, Identifier { Identifier.name; _ }) -> f acc name
      | (_, Object { Object.properties; _ }) -> List.fold_left (property f) acc properties
      | (_, Array { Array.elements; _ }) -> List.fold_left (element f) acc elements
      (* This is for assignment and default param destructuring `[a.b=1]=c`, ignore these for now. *)
      | (_, Expression _) -> acc
  )

let fold_bindings_of_variable_declarations f acc declarations =
  let open Flow_ast.Statement.VariableDeclaration in
  List.fold_left
    (fun acc -> function
      | (_, { Declarator.id = pattern; _ }) ->
        let has_anno =
          (* Only the toplevel annotation in a pattern is meaningful *)
          let open Flow_ast.Pattern in
          match pattern with
          | (_, Array { Array.annot = Flow_ast.Type.Available _; _ })
          | (_, Object { Object.annot = Flow_ast.Type.Available _; _ })
          | (_, Identifier { Identifier.annot = Flow_ast.Type.Available _; _ }) ->
            true
          | _ -> false
        in
        fold_bindings_of_pattern (f has_anno) acc pattern)
    acc
    declarations

let rec pattern_has_binding =
  let open Pattern in
  let property =
    let open Object in
    function
    | Property (_, { Property.pattern = p; _ })
    | RestElement (_, { RestElement.argument = p; comments = _ }) ->
      pattern_has_binding p
  in
  let element =
    let open Array in
    function
    | Hole _ -> false
    | Element (_, { Element.argument = p; default = _ })
    | RestElement (_, { RestElement.argument = p; comments = _ }) ->
      pattern_has_binding p
  in
  function
  | (_, Identifier _) -> true
  | (_, Object { Object.properties; _ }) -> List.exists property properties
  | (_, Array { Array.elements; _ }) -> List.exists element elements
  | (_, Expression _) -> false

let partition_directives statements =
  let open Flow_ast.Statement in
  let rec helper directives = function
    | ((_, Expression { Expression.directive = Some _; _ }) as directive) :: rest ->
      helper (directive :: directives) rest
    | rest -> (List.rev directives, rest)
  in
  helper [] statements

let hoist_function_and_component_declarations stmts =
  let open Flow_ast.Statement in
  let (func_and_component_decs, other_stmts) =
    List.partition
      (function
        (* function f() {} / component F() {} *)
        | (_, (FunctionDeclaration { Flow_ast.Function.id = Some _; _ } | ComponentDeclaration _))
        (* export function f() {} / export component F() {} *)
        | ( _,
            ExportNamedDeclaration
              {
                ExportNamedDeclaration.declaration =
                  Some
                    ( _,
                      ( FunctionDeclaration { Flow_ast.Function.id = Some _; _ }
                      | ComponentDeclaration _ )
                    );
                _;
              }
          )
        (* export default function f() {} / export default component F() {} *)
        | ( _,
            ExportDefaultDeclaration
              {
                ExportDefaultDeclaration.declaration =
                  ExportDefaultDeclaration.Declaration
                    ( _,
                      ( FunctionDeclaration { Flow_ast.Function.id = Some _; _ }
                      | ComponentDeclaration _ )
                    );
                _;
              }
          )
        (* TODO(jmbrown): Hoist declared components *)
        (* declare function f(): void; *)
        | (_, DeclareFunction _)
        (* declare export function f(): void; *)
        | ( _,
            DeclareExportDeclaration DeclareExportDeclaration.{ declaration = Some (Function _); _ }
          ) ->
          true
        | _ -> false)
      stmts
  in
  func_and_component_decs @ other_stmts

let negate_raw_lit raw =
  let raw_len = String.length raw in
  if raw_len > 0 && raw.[0] = '-' then
    String.sub raw 1 (raw_len - 1)
  else
    "-" ^ raw

let negate_number_literal (value, raw) = (~-.value, negate_raw_lit raw)

let negate_bigint_literal (value, raw) =
  match value with
  | None -> (None, raw)
  | Some value -> (Some (Int64.neg value), negate_raw_lit raw)

let is_call_to_invariant callee =
  match callee with
  | (_, Expression.Identifier (_, { Identifier.name = "invariant"; _ })) -> true
  | _ -> false

let is_call_to_is_array callee =
  match callee with
  | ( _,
      Flow_ast.Expression.Member
        {
          Flow_ast.Expression.Member._object =
            ( _,
              Flow_ast.Expression.Identifier
                (_, { Flow_ast.Identifier.name = "Array"; comments = _ })
            );
          property =
            Flow_ast.Expression.Member.PropertyIdentifier
              (_, { Flow_ast.Identifier.name = "isArray"; comments = _ });
          comments = _;
        }
    ) ->
    true
  | _ -> false

let is_call_to_object_dot_freeze callee =
  match callee with
  | ( _,
      Flow_ast.Expression.Member
        {
          Flow_ast.Expression.Member._object =
            ( _,
              Flow_ast.Expression.Identifier
                (_, { Flow_ast.Identifier.name = "Object"; comments = _ })
            );
          property =
            Flow_ast.Expression.Member.PropertyIdentifier
              (_, { Flow_ast.Identifier.name = "freeze"; comments = _ });
          comments = _;
        }
    ) ->
    true
  | _ -> false

let is_call_to_object_static_method callee =
  match callee with
  | ( _,
      Flow_ast.Expression.Member
        {
          Flow_ast.Expression.Member._object =
            ( _,
              Flow_ast.Expression.Identifier
                (_, { Flow_ast.Identifier.name = "Object"; comments = _ })
            );
          property = Flow_ast.Expression.Member.PropertyIdentifier _;
          comments = _;
        }
    ) ->
    true
  | _ -> false

let is_super_member_access = function
  | { Flow_ast.Expression.Member._object = (_, Flow_ast.Expression.Super _); _ } -> true
  | _ -> false

let loc_of_statement = fst

let loc_of_expression = fst

let loc_of_pattern = fst

let loc_of_ident = fst

let name_of_ident (_, { Identifier.name; comments = _ }) = name

let source_of_ident (loc, { Identifier.name; comments = _ }) = (loc, name)

let ident_of_source ?comments (loc, name) = (loc, { Identifier.name; comments })

let mk_comments ?(leading = []) ?(trailing = []) a = { Syntax.leading; trailing; internal = a }

let mk_comments_opt ?(leading = []) ?(trailing = []) () =
  match (leading, trailing) with
  | ([], []) -> None
  | (_, _) -> Some (mk_comments ~leading ~trailing ())

let mk_comments_with_internal_opt ?(leading = []) ?(trailing = []) ~internal () =
  match (leading, trailing, internal) with
  | ([], [], []) -> None
  | _ -> Some (mk_comments ~leading ~trailing internal)

let merge_comments ~inner ~outer =
  let open Syntax in
  match (inner, outer) with
  | (None, c)
  | (c, None) ->
    c
  | (Some inner, Some outer) ->
    mk_comments_opt
      ~leading:(outer.leading @ inner.leading)
      ~trailing:(inner.trailing @ outer.trailing)
      ()

let merge_comments_with_internal ~inner ~outer =
  match (inner, outer) with
  | (inner, None) -> inner
  | (None, Some { Syntax.leading; trailing; _ }) ->
    mk_comments_with_internal_opt ~leading ~trailing ~internal:[] ()
  | ( Some { Syntax.leading = inner_leading; trailing = inner_trailing; internal },
      Some { Syntax.leading = outer_leading; trailing = outer_trailing; _ }
    ) ->
    mk_comments_with_internal_opt
      ~leading:(outer_leading @ inner_leading)
      ~trailing:(inner_trailing @ outer_trailing)
      ~internal
      ()

let split_comments comments =
  match comments with
  | None -> (None, None)
  | Some { Syntax.leading; trailing; _ } ->
    (mk_comments_opt ~leading (), mk_comments_opt ~trailing ())

let string_of_assignment_operator op =
  let open Flow_ast.Expression.Assignment in
  match op with
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | MultAssign -> "*="
  | ExpAssign -> "**="
  | DivAssign -> "/="
  | ModAssign -> "%="
  | LShiftAssign -> "<<="
  | RShiftAssign -> ">>="
  | RShift3Assign -> ">>>="
  | BitOrAssign -> "|="
  | BitXorAssign -> "^="
  | BitAndAssign -> "&="
  | NullishAssign -> "??="
  | AndAssign -> "&&="
  | OrAssign -> "||="

let string_of_binary_operator op =
  let open Flow_ast.Expression.Binary in
  match op with
  | Equal -> "=="
  | NotEqual -> "!="
  | StrictEqual -> "==="
  | StrictNotEqual -> "!=="
  | LessThan -> "<"
  | LessThanEqual -> "<="
  | GreaterThan -> ">"
  | GreaterThanEqual -> ">="
  | LShift -> "<<"
  | RShift -> ">>"
  | RShift3 -> ">>>"
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Exp -> "**"
  | Div -> "/"
  | Mod -> "%"
  | BitOr -> "|"
  | Xor -> "^"
  | BitAnd -> "&"
  | In -> "in"
  | Instanceof -> "instanceof"

module ExpressionSort = struct
  type t =
    | Array
    | ArrowFunction
    | Assignment
    | Binary
    | Call
    | Class
    | Conditional
    | Function
    | Identifier
    | Import
    | JSXElement
    | JSXFragment
    | Literal
    | Logical
    | Member
    | MetaProperty
    | New
    | Object
    | OptionalCall
    | OptionalMember
    | Sequence
    | Super
    | TaggedTemplate
    | TemplateLiteral
    | This
    | TypeCast
    | Unary
    | Update
    | Yield
  [@@deriving show]

  let to_string = function
    | Array -> "array"
    | ArrowFunction -> "arrow function"
    | Assignment -> "assignment expression"
    | Binary -> "binary expression"
    | Call -> "call expression"
    | Class -> "class"
    | Conditional -> "conditional expression"
    | Function -> "function"
    | Identifier -> "identifier"
    | Import -> "import expression"
    | JSXElement -> "JSX element"
    | JSXFragment -> "JSX fragment"
    | Literal -> "literal"
    | Logical -> "logical expression"
    | Member -> "member expression"
    | MetaProperty -> "metaproperty expression"
    | New -> "new expression"
    | Object -> "object"
    | OptionalCall -> "optional call expression"
    | OptionalMember -> "optional member expression"
    | Sequence -> "sequence"
    | Super -> "`super` reference"
    | TaggedTemplate -> "tagged template expression"
    | TemplateLiteral -> "template literal"
    | This -> "`this` reference"
    | TypeCast -> "type cast"
    | Unary -> "unary expression"
    | Update -> "update expression"
    | Yield -> "yield expression"
end

let loc_of_annotation_or_hint =
  let open Flow_ast.Type in
  function
  | Missing loc
  | Available (_, (loc, _)) ->
    loc

let loc_of_return_annot =
  let open Flow_ast.Function.ReturnAnnot in
  function
  | Missing loc
  | Available (_, (loc, _))
  | TypeGuard (loc, _) ->
    loc

(* Apply type [t] at the toplevel of expression [exp]. This is straightforward overall
 * except for the case of Identifier and Member, where we push the type within the
 * identifier and member property type position as well. This is to ensure that
 * type-at-pos searcher will detect the updated type. *)
let push_toplevel_type t exp =
  let open Flow_ast.Expression in
  let push_toplevel_identifier id =
    let ((id_loc, _), id) = id in
    ((id_loc, t), id)
  in
  let push_to_member mem =
    match mem with
    | { Member.property = Member.PropertyIdentifier id; _ } ->
      { mem with Member.property = Member.PropertyIdentifier (push_toplevel_identifier id) }
    | p -> p
  in
  let ((loc, _), e) = exp in
  let e' =
    match e with
    | Identifier id -> Identifier (push_toplevel_identifier id)
    | Member member -> Member (push_to_member member)
    | OptionalMember ({ OptionalMember.member; _ } as omem) ->
      OptionalMember { omem with OptionalMember.member = push_to_member member }
    | _ -> e
  in
  ((loc, t), e')

let hook_name s =
  let is_A_to_Z c = c >= 'A' && c <= 'Z' in
  String.starts_with ~prefix:"use" s && (String.length s = 3 || is_A_to_Z s.[3])

let hook_function { Flow_ast.Function.id; _ } =
  match id with
  | Some (loc, { Flow_ast.Identifier.name; _ }) when hook_name name -> Some loc
  | _ -> None

let hook_call { Flow_ast.Expression.Call.callee; _ } =
  (* A.B.C.useFoo() is a hook, A().useFoo() is not *)
  let open Flow_ast.Expression in
  let rec hook_callee top exp =
    match exp with
    | (_, Identifier (_, { Flow_ast.Identifier.name; _ })) -> hook_name name || not top
    | ( _,
        Member
          {
            Member._object;
            property = Member.PropertyIdentifier (_, { Flow_ast.Identifier.name; _ });
            _;
          }
      ) ->
      (hook_name name || not top) && hook_callee false _object
    | _ -> false
  in
  hook_callee true callee
