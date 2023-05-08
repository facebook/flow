// @flow

type O = {| foo: number, bar: string |};

// Concrete - should be evaluated before normalization

type MappedObj1 = {[key in keyof O]: number};
//   ^
type MappedObj2 = {[key in keyof O]: O[key]};
//   ^
type MappedObj3 = {-[key in keyof O]: O[key]};
//   ^
type MappedObj4 = {+[key in keyof O]: O[key]};
//   ^
type MappedObj5 = {[key in keyof O]?: O[key]};
//   ^
type MappedObj6 = {+[key in keyof O]?: O[key]};
//   ^
type MappedObj7 = {-[key in keyof O]?: O[key]};
//   ^
type DifferingModifiers = {foo?: number, -bar: string, +baz: string, ...};
// We can't use O[key] on write-only properties, so using number here.
type MappedObj8 = {[key in keyof DifferingModifiers]: number};
//   ^
// -? Doesn't work yet for type checking so we print any here
type MappedObj9 = {[key in keyof O]-?: number};
//   ^

// Unevaluated
type Unevaluated1<T: {...}> = {[key in keyof T]: T[key]}; 
//   ^
type Unevaluated2<T: {...}> = {+[key in keyof T]: T[key]}; 
//   ^
type Unevaluated3<T: {...}> = {-[key in keyof T]: T[key]}; 
//   ^
type Unevaluated4<T: {...}> = {[key in keyof T]?: T[key]}; 
//   ^
type Unevaluated5<T: {...}> = {+[key in keyof T]?: T[key]}; 
//   ^
type Unevaluated6<T: {...}> = {-[key in keyof T]?: T[key]}; 
//   ^

declare var x: {[key in keyof O]: O[key]};
   x;
// ^

type Keys = 'foo' | 'bar';

// Concrete - should be evaluated before normalization

type MappedObjNonHomomorphic1 = {[key in Keys]: number};
//   ^
type MappedObjNonHomomorphic2 = {[key in Keys]: O[key]};
//   ^
type MappedObjNonHomomorphic3 = {-[key in Keys]: O[key]};
//   ^
type MappedObjNonHomomorphic4 = {+[key in Keys]: O[key]};
//   ^
type MappedObjNonHomomorphic5 = {[key in Keys]?: O[key]};
//   ^
type MappedObjNonHomomorphic6 = {+[key in Keys]?: O[key]};
//   ^
type MappedObjNonHomomorphic7 = {-[key in Keys]?: O[key]};
//   ^

// Unevaluated
type UnevaluatedNonHomomorphic<T: {...}> = {[key in T]: T};
//   ^

declare var y: {[key in Keys]: O[key]};
   x;
// ^
