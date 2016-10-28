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
    | Schema of SchemaDef.t
end = Definition

and OperationDef : sig
  type t = {
    operation: OperationType.op;
    name: Name.t option;
    selectionSet: SelectionSet.t;
    variableDefs: VariableDef.t list option;
    directives: Directive.t list option;
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
  }
end = ObjectTypeDef

and FieldDef : sig
  type t = {
    name: Name.t;
    type_: Type.t;
    args: InputValueDef.t list;
    directives: Directive.t list option;
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
  }
end = InterfaceTypeDef

and UnionTypeDef : sig
  type t = {
    name: Name.t;
    types: Name.t list;
    directives: Directive.t list option;
  }
end = UnionTypeDef

and EnumTypeDef : sig
  type t = {
    name: Name.t;
    values: EnumValueDef.t list;
    directives: Directive.t list option;
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
  }
end = InputObjectTypeDef

and Value : sig
  type t =
    | Variable of Variable.t
    | IntValue of Loc.t * string
    | FloatValue of Loc.t * string
    | StringValue of Loc.t * string
    | BooleanValue of Loc.t * bool
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
    operationTypes: OperationType.t list;
    directives: Directive.t list option;
  }
end = SchemaDef

and OperationType : sig
  type op = Query | Mutation | Subscription

  type t = {
    operation: op;
    type_: Name.t;
  }
end = OperationType

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
  }
end = SelectionSet

and Selection : sig
  type t =
    | Field of Field.t
    | FragmentSpread of FragmentSpread.t
    | InlineFragment of InlineFragment.t
end = Selection

and Field : sig
  type t = {
    alias: Name.t option;
    name: Name.t;
    args: Argument.t list option;
    selectionSet: SelectionSet.t option;
    directives: Directive.t list option;
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
  }
end = FragmentSpread

and InlineFragment : sig
  type t = {
    typeCondition: Name.t option;
    selectionSet: SelectionSet.t;
    directives: Directive.t list option;
  }
end = InlineFragment
