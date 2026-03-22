type T1 = ({foo: string, bar: number}) & {baz: boolean};
type T2 = ({type: "a"} | {type: "b"});
type T3 = ({} extends Props ? null : never);
type T4 = ({} & string) | "literal";
type T5 = ([number, string]) & {extra: boolean};
