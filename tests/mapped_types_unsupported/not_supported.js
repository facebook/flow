type O = {foo: number};
type X = {[key in keyof O]: number}; // ERROR mapped types not yet supported
// Additional internal errors fixed in the next diff
