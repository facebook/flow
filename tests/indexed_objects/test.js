//@flow
const x: {| [string]: number |} = {foo: 3}; // Ignores exact bars

type Y = {| ...{[string]: number}, foo: number|};
const y: Y = {foo: 3}; // No error

type Z = $Exact<{[string]: number}>;
const z: Z = {foo: 3}; // No error
