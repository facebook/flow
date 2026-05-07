//@flow

declare const f : {foo: number, bar: string, 'foo.bar': boolean} => void;

f({ '
//   ^
