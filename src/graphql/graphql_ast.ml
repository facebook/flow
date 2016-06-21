module JSAst = Spider_monkey_ast

module rec Node : sig
  type t =
    | Name of Name.t
    | Document of Document.t
    | OperationDefinition of OperationDefinition.t
    | Field of Field.t
    | SelectionSet of SelectionSet.t
end = Node

and Name : sig
  type t = Loc.t * string
end = Name

and Document : sig
  type t = {
    definitions: t' list;
  }
  and t' =
    | OperationDefinition of OperationDefinition.t
    | FragmentDefinition of FragmentDefinition.t
end = Document

and OperationDefinition : sig
  type t = {
    operation: operationType;
    name: string;
    selectionSet: SelectionSet.t;
  }
  and operationType =
    | Query
    | Mutation
    | Subscribtion
end = OperationDefinition

and FragmentDefinition : sig
  type t = {
    name: Name.t;
    typeName: Name.t;
    selectionSet: SelectionSet.t;
  }
end = FragmentDefinition

and Field : sig
  type t = {
    name: string;
    selectionSet: SelectionSet.t option;
    loc: Loc.t;
  }
end = Field

and SelectionSet : sig
  type t = {
    selections: t' list;
  }
  and t' =
    | Field of Field.t
    | Expression of JSAst.Expression.t
end = SelectionSet

let find_op_def doc =
  match List.nth (doc.Document.definitions) 0 with
    | Document.OperationDefinition op_def -> op_def
    | _ -> failwith "Not operation definition"

let fields_of_op op_def = op_def.OperationDefinition.selectionSet
