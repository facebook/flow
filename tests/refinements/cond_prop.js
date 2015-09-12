/* @flow */

type Type = Name | ListType | NonNullType;
type Name = {kind: 'Name', value: string};
type ListType = {kind: 'ListType', type: Type};
type NonNullType = {kind: 'NonNullType', type: Name | ListType | BadType};
type BadType = {};

function getTypeASTName(typeAST: Type): string {
  invariant(typeAST.type, 'Must be wrapping type'); // OK
  return getTypeASTName(typeAST.type); // error, BadType not a subtype of Type
}
