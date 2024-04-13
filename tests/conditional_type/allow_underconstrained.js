type Helper1<T> = T extends null | [infer X] ? X : number;
type Foo = Helper1<null>; // ok
1 as Foo; // ok: Foo = mixed

type Helper2<T> = T extends null | [infer X extends string] ? X : number;
type Bar = Helper2<null>; // ok
1 as Bar; // error: Bar = string
