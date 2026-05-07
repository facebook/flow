type Literal = 'foo';
declare class Wrapper<T> {}

declare const a: Wrapper<Literal>;
let reassigned = a;
reassigned = new Wrapper<Literal>();
