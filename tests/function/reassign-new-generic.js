type Literal = 'foo';
declare class Wrapper<T> {}

declare var a: Wrapper<Literal>;
let reassigned = a;
reassigned = new Wrapper<Literal>();
