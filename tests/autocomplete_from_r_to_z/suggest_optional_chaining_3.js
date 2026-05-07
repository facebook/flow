//@flow

declare const x : (?{foo : string} & {}) | (?{foo : number} & {});
x.
//^
