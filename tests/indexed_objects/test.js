//@flow
const x: {| [string]: number |} = {foo: 3}; // Ignores exact bars

type Y = {| ...{[string]: number}, foo: number|};
const y: Y = {foo: 3}; // No error

type Z = $Exact<{[string]: number}>;
const z: Z = {foo: 3}; // No error

type Props = {
  [StringPrefix<'data-'>]: string | void,
}

const error1: Props = {foo: 3}; // One error

type ObjWithProto = {
  __proto__: {foo: number},
  [StringPrefix<'bar'>]: string,
};

declare const objWithProto: ObjWithProto;
objWithProto.foo as number; // TODO: no error
