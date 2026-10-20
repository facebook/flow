type ObjectType = ({foo: string, bar: number}) & {baz: boolean};
type Union = ({type: "a"} | {type: "b"});
type Conditional = ({} extends Props ? null : never);
type Intersection = ({} & string) | "literal";
type TupleType = ([number, string]) & {extra: boolean};
