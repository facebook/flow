(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast
module E = Expression
module I = Identifier

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
  let open Statement.VariableDeclaration in
  List.fold_left
    (fun acc -> function
      | (_, { Declarator.id = pattern; _ }) ->
        let has_anno =
          (* Only the toplevel annotation in a pattern is meaningful *)
          let open Pattern in
          match pattern with
          | (_, Array { Array.annot = Type.Available _; _ })
          | (_, Object { Object.annot = Type.Available _; _ })
          | (_, Identifier { Identifier.annot = Type.Available _; _ }) ->
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

let rec match_pattern_has_binding =
  let open MatchPattern in
  let property = function
    | (_, ObjectPattern.Property.Valid { ObjectPattern.Property.pattern = p; _ }) ->
      match_pattern_has_binding p
    | (_, ObjectPattern.Property.InvalidShorthand _) -> false
  in
  let rest_has_binding = function
    | Some (_, { RestPattern.argument = Some _; comments = _ }) -> true
    | _ -> false
  in
  function
  | (_, WildcardPattern _)
  | (_, NumberPattern _)
  | (_, BigIntPattern _)
  | (_, StringPattern _)
  | (_, BooleanPattern _)
  | (_, NullPattern _)
  | (_, UnaryPattern _)
  | (_, IdentifierPattern _)
  | (_, MemberPattern _) ->
    false
  | (_, BindingPattern _) -> true
  | (_, ObjectPattern { ObjectPattern.properties; rest; comments = _ }) ->
    rest_has_binding rest || List.exists property properties
  | (_, ArrayPattern { ArrayPattern.elements; rest; comments = _ }) ->
    rest_has_binding rest
    || List.exists
         (fun { ArrayPattern.Element.pattern; _ } -> match_pattern_has_binding pattern)
         elements
  | (_, OrPattern { OrPattern.patterns; _ }) -> List.exists match_pattern_has_binding patterns
  | (_, AsPattern _) -> true

let string_of_variable_kind = function
  | Variable.Var -> "var"
  | Variable.Let -> "let"
  | Variable.Const -> "const"

let partition_directives statements =
  let open Statement in
  let rec helper directives = function
    | ((_, Expression { Expression.directive = Some _; _ }) as directive) :: rest ->
      helper (directive :: directives) rest
    | rest -> (List.rev directives, rest)
  in
  helper [] statements

let hoist_function_and_component_declarations stmts =
  let open Statement in
  let (func_and_component_decs, other_stmts) =
    List.partition
      (function
        (* function f() {} / component F() {} *)
        | (_, (FunctionDeclaration { Function.id = Some _; _ } | ComponentDeclaration _))
        (* export function f() {} / export component F() {} *)
        | ( _,
            ExportNamedDeclaration
              {
                ExportNamedDeclaration.declaration =
                  Some
                    (_, (FunctionDeclaration { Function.id = Some _; _ } | ComponentDeclaration _));
                _;
              }
          )
        (* export default function f() {} / export default component F() {} *)
        | ( _,
            ExportDefaultDeclaration
              {
                ExportDefaultDeclaration.declaration =
                  ExportDefaultDeclaration.Declaration
                    (_, (FunctionDeclaration { Function.id = Some _; _ } | ComponentDeclaration _));
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

let is_number_literal node =
  match node with
  | Expression.NumberLiteral _
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.NumberLiteral _);
        comments = _;
      } ->
    true
  | _ -> false

let extract_number_literal node =
  match node with
  | Expression.NumberLiteral { NumberLiteral.value; raw; comments = _ } -> Some (value, raw)
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.NumberLiteral { NumberLiteral.value; raw; _ });
        comments = _;
      } ->
    Some (negate_number_literal (value, raw))
  | _ -> None

let is_bigint_literal node =
  match node with
  | Expression.BigIntLiteral _
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.BigIntLiteral _);
        comments = _;
      } ->
    true
  | _ -> false

let extract_bigint_literal node =
  match node with
  | Expression.BigIntLiteral { BigIntLiteral.value; raw; comments = _ } -> Some (value, raw)
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.BigIntLiteral { BigIntLiteral.value; raw; comments = _ });
        comments = _;
      } ->
    Some (negate_bigint_literal (value, raw))
  | _ -> None

let is_call_to_invariant callee =
  match callee with
  | (_, Expression.Identifier (_, { Identifier.name = "invariant"; _ })) -> true
  | _ -> false

let is_call_to_require callee =
  match callee with
  | (_, Expression.Identifier (_, { Identifier.name = "require"; _ })) -> true
  | _ -> false

let is_call_to_is_array callee =
  match callee with
  | ( _,
      E.Member
        {
          E.Member._object = (_, E.Identifier (_, { I.name = "Array"; comments = _ }));
          property = E.Member.PropertyIdentifier (_, { I.name = "isArray"; comments = _ });
          comments = _;
        }
    ) ->
    true
  | _ -> false

let is_call_to_object_dot_freeze callee =
  match callee with
  | ( _,
      E.Member
        {
          E.Member._object = (_, E.Identifier (_, { I.name = "Object"; comments = _ }));
          property = E.Member.PropertyIdentifier (_, { I.name = "freeze"; comments = _ });
          comments = _;
        }
    ) ->
    true
  | _ -> false

let get_call_to_object_dot_freeze_arg callee targs args =
  match (targs, args) with
  | (None, (_args_loc, { E.ArgList.arguments = [E.Expression (obj_loc, E.Object o)]; comments = _ }))
    when is_call_to_object_dot_freeze callee ->
    Some (obj_loc, o)
  | _ -> None

let is_call_to_object_static_method callee =
  match callee with
  | ( _,
      E.Member
        {
          E.Member._object = (_, E.Identifier (_, { I.name = "Object"; comments = _ }));
          property = E.Member.PropertyIdentifier _;
          comments = _;
        }
    ) ->
    true
  | _ -> false

let is_module_dot_exports callee =
  match callee with
  | ( _,
      E.Member
        {
          E.Member._object = (_, E.Identifier (_, { I.name = "module"; comments = _ }));
          property = E.Member.PropertyIdentifier (_, { I.name = "exports"; comments = _ });
          comments = _;
        }
    ) ->
    true
  | _ -> false

let get_call_to_jest_module_mocking_fn callee arguments =
  match (callee, arguments) with
  | ( ( _,
        E.Member
          {
            E.Member._object = (_, E.Identifier (jest_loc, { I.name = "jest"; comments = _ }));
            property =
              E.Member.PropertyIdentifier
                ( _,
                  (* See https://jestjs.io/docs/jest-object#mock-modules *)
                  {
                    I.name =
                      ( "createMockFromModule" | "mock" | "unmock" | "deepUnmock" | "doMock"
                      | "dontMock" | "setMock" | "requireActual" | "requireMock" );
                    comments = _;
                  }
                );
            _;
          }
      ),
      ( _,
        {
          E.ArgList.arguments =
            E.Expression
              ( source_loc,
                ( E.StringLiteral { StringLiteral.value = name; _ }
                | E.TemplateLiteral
                    {
                      E.TemplateLiteral.quasis =
                        [
                          ( _,
                            {
                              E.TemplateLiteral.Element.value =
                                { E.TemplateLiteral.Element.cooked = name; _ };
                              _;
                            }
                          );
                        ];
                      _;
                    } )
              )
            :: _;
          comments = _;
        }
      )
    ) ->
    Some (jest_loc, source_loc, name)
  | _ -> None

let is_super_member_access = function
  | { E.Member._object = (_, E.Super _); _ } -> true
  | _ -> false

let acceptable_statement_in_declaration_context ~in_declare_namespace =
  let open Statement in
  function
  | Block _ -> Error "block"
  | Break _ -> Error "break"
  | ClassDeclaration _ -> Error "class declaration"
  | ComponentDeclaration _ -> Error "component declaration"
  | Continue _ -> Error "continue"
  | Debugger _ -> Error "debugger"
  | DoWhile _ -> Error "do while"
  | ExportDefaultDeclaration _ -> Error "export default"
  | ExportNamedDeclaration { ExportNamedDeclaration.export_kind = ExportValue; _ } ->
    Error "value export"
  | Expression _ -> Error "expression"
  | For _ -> Error "for"
  | ForIn _ -> Error "for in"
  | ForOf _ -> Error "for of"
  | FunctionDeclaration _ -> Error "function declaration"
  | If _ -> Error "if"
  | Labeled _ -> Error "labeled"
  | Match _ -> Error "match"
  | Return _ -> Error "return"
  | Switch _ -> Error "switch"
  | Throw _ -> Error "throw"
  | Try _ -> Error "try"
  | VariableDeclaration _ -> Error "variable declaration"
  | While _ -> Error "while"
  | With _ -> Error "with"
  | ImportDeclaration _ ->
    if in_declare_namespace then
      Error "import declaration"
    else
      Ok ()
  | DeclareModuleExports _ ->
    if in_declare_namespace then
      Error "declare module.exports"
    else
      Ok ()
  | DeclareClass _
  | DeclareComponent _
  | DeclareEnum _
  | DeclareExportDeclaration _
  | DeclareFunction _
  | DeclareInterface _
  | DeclareModule _
  | DeclareNamespace _
  | DeclareOpaqueType _
  | DeclareTypeAlias _
  | DeclareVariable _
  | Empty _
  | EnumDeclaration _
  | ExportNamedDeclaration { ExportNamedDeclaration.export_kind = ExportType; _ }
  | InterfaceDeclaration _
  | OpaqueType _
  | TypeAlias _ ->
    Ok ()

let rec is_type_only_declaration_statement (_, stmt') =
  let open Statement in
  let is_type_only_declaration_statement' = function
    | DeclareInterface _
    | DeclareOpaqueType _
    | DeclareTypeAlias _
    | Empty _
    | InterfaceDeclaration _
    | OpaqueType _
    | TypeAlias _ ->
      true
    | DeclareExportDeclaration
        DeclareExportDeclaration.
          { declaration = Some (NamedType _ | NamedOpaqueType _ | Interface _); _ } ->
      true
    | DeclareNamespace { DeclareNamespace.body = (_, { Block.body; _ }); _ } ->
      List.for_all is_type_only_declaration_statement body
    | ExportNamedDeclaration { ExportNamedDeclaration.export_kind; _ } -> export_kind = ExportType
    | Block _
    | Break _
    | ClassDeclaration _
    | ComponentDeclaration _
    | Continue _
    | Debugger _
    | DoWhile _
    | EnumDeclaration _
    | ExportDefaultDeclaration _
    | Expression _
    | For _
    | ForIn _
    | ForOf _
    | FunctionDeclaration _
    | If _
    | Labeled _
    | Match _
    | Return _
    | Switch _
    | Throw _
    | Try _
    | VariableDeclaration _
    | While _
    | With _
    | ImportDeclaration _
    | DeclareClass _
    | DeclareComponent _
    | DeclareEnum _
    | DeclareExportDeclaration _
    | DeclareFunction _
    | DeclareModule _
    | DeclareModuleExports _
    | DeclareVariable _ ->
      false
  in
  is_type_only_declaration_statement' stmt'

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
  let open E.Assignment in
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
  let open E.Binary in
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
    | Match
    | Member
    | MetaProperty
    | New
    | Object
    | OptionalCall
    | OptionalMember
    | Satisfies
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
    | Match -> "match expression"
    | Member -> "member expression"
    | MetaProperty -> "metaproperty expression"
    | New -> "new expression"
    | Object -> "object"
    | OptionalCall -> "optional call expression"
    | OptionalMember -> "optional member expression"
    | Satisfies -> "satisfies expression"
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
  let open Type in
  function
  | Missing loc
  | Available (_, (loc, _)) ->
    loc

let loc_of_return_annot =
  let open Function.ReturnAnnot in
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
  let open E in
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
  let is_cap c = c = Char.uppercase_ascii c in
  String.starts_with ~prefix:"use" s && (String.length s = 3 || is_cap s.[3])

let hook_function { Function.id; _ } =
  match id with
  | Some (loc, { I.name; _ }) when hook_name name -> Some loc
  | _ -> None

let hook_call { E.Call.callee; _ } =
  (* A.B.C.useFoo() is a hook, A().useFoo() is not *)
  let open E in
  let rec hook_callee top exp =
    match exp with
    | (_, Identifier (_, { I.name; _ })) -> hook_name name || not top
    | (_, Member { Member._object; property = Member.PropertyIdentifier (_, { I.name; _ }); _ }) ->
      (hook_name name || not top) && hook_callee false _object
    | _ -> false
  in
  hook_callee true callee

(* Match *)
let match_root_name = "<match_root>"

let match_root_ident loc = (loc, { Identifier.name = match_root_name; comments = None })

let expression_of_match_member_pattern ~visit_expression pattern =
  let open MatchPattern in
  let rec f (loc, { MemberPattern.base; property; comments }) =
    let (_object, root_name) =
      match base with
      | MemberPattern.BaseIdentifier ((loc, _) as id) -> ((loc, Expression.Identifier id), id)
      | MemberPattern.BaseMember mem -> f mem
    in
    let property =
      match property with
      | MemberPattern.PropertyIdentifier id -> Expression.Member.PropertyIdentifier id
      | MemberPattern.PropertyString (loc, lit) ->
        Expression.Member.PropertyExpression (loc, Expression.StringLiteral lit)
      | MemberPattern.PropertyNumber (loc, lit) ->
        Expression.Member.PropertyExpression (loc, Expression.NumberLiteral lit)
      | MemberPattern.PropertyBigInt (loc, lit) ->
        Expression.Member.PropertyExpression (loc, Expression.BigIntLiteral lit)
    in
    let exp = (loc, Expression.Member { Expression.Member._object; property; comments }) in
    visit_expression exp;
    (exp, root_name)
  in
  f pattern

(* Type Guards *)
let get_inferred_type_guard_candidate params body return =
  match (body, return) with
  | (Function.BodyExpression _, Function.ReturnAnnot.Missing _) -> begin
    match params with
    | ( _,
        {
          Function.Params.params =
            [
              ( _,
                {
                  Function.Param.argument =
                    ( _,
                      Pattern.Identifier
                        { Pattern.Identifier.name = (loc, { Identifier.name; _ }); _ }
                    );
                  _;
                }
              );
            ];
          rest = None;
          _;
        }
      ) ->
      Some (loc, name)
    | _ -> None
  end
  | _ -> None

let rec unwrap_nonnull_lhs_expr :
          'loc 'tloc1 'tloc2.
          ('loc, 'tloc1) Expression.t ->
          ('loc, 'tloc1) Expression.t
          * bool
          * (('loc, 'tloc2) Expression.t ->
            filter_nullish:('tloc1 -> 'tloc2) ->
            ('loc, 'tloc2) Expression.t
            ) =
 fun expr ->
  match expr with
  | ( loc,
      Expression.Unary { Expression.Unary.operator = Expression.Unary.Nonnull; argument; comments }
    ) ->
    let (argument, _, reconstruct) = unwrap_nonnull_lhs_expr argument in
    let reconstruct argument ~filter_nullish =
      ( filter_nullish loc,
        Expression.Unary
          {
            Expression.Unary.operator = Expression.Unary.Nonnull;
            argument = reconstruct ~filter_nullish argument;
            comments;
          }
      )
    in
    (argument, true, reconstruct)
  | _ -> (expr, false, (fun argument ~filter_nullish:_ -> argument))

let unwrap_nonnull_lhs : 'loc 'tloc. ('loc, 'tloc) Pattern.t -> ('loc, 'tloc) Pattern.t * bool =
 fun pat ->
  match pat with
  | (loc, Pattern.Expression expr) ->
    let (expr, optional, _) = unwrap_nonnull_lhs_expr expr in
    begin
      match expr with
      | (e_loc, Expression.Identifier name) ->
        assert optional;
        ( ( loc,
            Pattern.Identifier
              { Pattern.Identifier.name; optional = false; annot = Type.Missing e_loc }
          ),
          true
        )
      | _ -> ((loc, Pattern.Expression expr), optional)
    end
  | _ -> (pat, false)
