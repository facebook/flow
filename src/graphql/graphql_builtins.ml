let builtins_str = {|
  scalar Int
  scalar Float
  scalar String
  scalar Boolean
  scalar ID

  type __Schema {
    types: [__Type!]!
    queryType: __Type!
    mutationType: __Type
    directives: [__Directive!]!
  }

  type __Type {
    kind: __TypeKind!
    name: String
    description: String

    fields(includeDeprecated: Boolean = false): [__Field!]

    interfaces: [__Type!]

    possibleTypes: [__Type!]

    enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

    inputFields: [__InputValue!]

    ofType: __Type
  }

  type __Field {
    name: String!
    description: String
    args: [__InputValue!]!
    type: __Type!
    isDeprecated: Boolean!
    deprecationReason: String
  }

  type __InputValue {
    name: String!
    description: String
    type: __Type!
    defaultValue: String
  }

  type __EnumValue {
    name: String!
    description: String
    isDeprecated: Boolean!
    deprecationReason: String
  }

  enum __TypeKind {
    SCALAR
    OBJECT
    INTERFACE
    UNION
    ENUM
    INUT_OBJECT
    LIST
    NON_NULL
  }

  type __Directive {
    name: String!
    description: String
    locations: [__DirectiveLocation!]!
    args: [__InputValue!]!
  }

  enum __DirectiveLocation {
    QUERY
    MUTATION
    FIELD
    FRAGMENT_DEFINITION
    FRAGMENT_SPREAD
    INLINE_FRAGMENT
  }
|}

let get_ast () =
  let lexbuf = Lexing.from_string builtins_str in
  Graphql_parser.doc Graphql_lexer.token lexbuf
