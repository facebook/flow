module rec Document : sig
  type t = {
    definitions: Definition.t list;
  }
end = Document

and Definition : sig

  type t =
    | Operation of OperationDef.t
    | Fragment of FragmentDef.t
    | ScalarType of ScalarTypeDef.t
    | ObjectType of ObjectTypeDef.t
    | InterfaceType of InterfaceTypeDef.t
    | UnionType of UnionTypeDef.t
    | EnumType of EnumTypeDef.t
    | InputObjectType of InputObjectTypeDef.t
    | TypeExtension of TypeExtensionDef.t
    | Directive of DirectiveDef.t
    | Schema of SchemaDef.t
end = Definition

and OperationDef : sig
  type t = {
    operation: OperationType.t;
    name: Name.t option;
    selectionSet: SelectionSet.t;
    variableDefs: VariableDef.t list option;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = OperationDef

and VariableDef : sig
  type t = {
    variable: Variable.t;
    type_: Type.t;
    defaultValue: Value.t option;
    loc: Loc.t;
  }
end = VariableDef

and Variable : sig
  type t = Loc.t * Name.t
end = Variable

and FragmentDef : sig
  type t = {
    name: Name.t option;
    typeCondition: Name.t;
    selectionSet: SelectionSet.t;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = FragmentDef

and ScalarTypeDef : sig
  type t = {
    name: Name.t;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = ScalarTypeDef

and ObjectTypeDef : sig
  type t = {
    name: Name.t;
    fields: FieldDef.t list;
    interfaces: Name.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = ObjectTypeDef

and FieldDef : sig
  type t = {
    name: Name.t;
    type_: Type.t;
    args: InputValueDef.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = FieldDef

and InputValueDef : sig
  type t = {
    name: Name.t;
    type_: Type.t;
    defaultValue: Value.t option;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = InputValueDef

and InterfaceTypeDef : sig
  type t = {
    name: Name.t;
    fields: FieldDef.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = InterfaceTypeDef

and UnionTypeDef : sig
  type t = {
    name: Name.t;
    types: Name.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = UnionTypeDef

and EnumTypeDef : sig
  type t = {
    name: Name.t;
    values: EnumValueDef.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = EnumTypeDef

and EnumValueDef : sig
  type t = {
    name: Name.t;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = EnumValueDef

and InputObjectTypeDef : sig
  type t = {
    name: Name.t;
    fields: InputValueDef.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = InputObjectTypeDef

and Value : sig
  type t =
    | Variable of Variable.t
    | IntValue of Loc.t * string
    | FloatValue of Loc.t * string
    | StringValue of Loc.t * string
    | BooleanValue of Loc.t * bool
    | NullValue of Loc.t
    | EnumValue of Loc.t * string
    | ListValue of Loc.t * Value.t list
    | ObjectValue of ObjectValue.t
end = Value

and ObjectValue : sig
  type t = {
    fields: ObjectField.t list;
    loc: Loc.t;
  }
end = ObjectValue

and ObjectField : sig
  type t = {
    name: Name.t;
    value: Value.t;
    loc: Loc.t;
  }
end = ObjectField

and Directive : sig
  type t = {
    name: Name.t;
    arguments: Argument.t list option;
    loc: Loc.t;
  }
end = Directive

and SchemaDef : sig
  type t = {
    operationTypes: OperationTypeDef.t list;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = SchemaDef

and OperationTypeDef : sig
  type t = {
    operation: OperationType.t;
    type_: Name.t;
    loc: Loc.t;
  }
end = OperationTypeDef

and OperationType : sig
  type t' = Query | Mutation | Subscription

  type t = Loc.t option * t'
end = OperationType

and TypeExtensionDef : sig
  type t = {
    definition: ObjectTypeDef.t;
    loc: Loc.t;
  }
end = TypeExtensionDef

and DirectiveDef : sig
  type t = {
    name: Name.t;
    arguments: InputValueDef.t list;
    locations: Name.t list;
    loc: Loc.t;
  }
end = DirectiveDef

and Name : sig
  type t = Loc.t * string
end = Name

and Type : sig
  type t =
    | Named of Name.t
    | List of Loc.t * t
    | NonNull of Loc.t * t
end = Type

and SelectionSet : sig
  type t = {
    selections: Selection.t list;
    loc: Loc.t;
  }
end = SelectionSet

and Selection : sig
  type t =
    | Field of Field.t
    | FragmentSpread of FragmentSpread.t
    | InlineFragment of InlineFragment.t
    | JS of Type.t
end = Selection

and Field : sig
  type t = {
    alias: Name.t option;
    name: Name.t;
    args: Argument.t list option;
    selectionSet: SelectionSet.t option;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = Field

and Argument : sig
  type t = {
    name: Name.t;
    value: Value.t;
    loc: Loc.t;
  }
end = Argument

and FragmentSpread : sig
  type t = {
    name: Name.t;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = FragmentSpread

and InlineFragment : sig
  type t = {
    typeCondition: Name.t option;
    selectionSet: SelectionSet.t;
    directives: Directive.t list option;
    loc: Loc.t;
  }
end = InlineFragment
