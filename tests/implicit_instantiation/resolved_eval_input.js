type Props<C> = C extends (props: infer P) => unknown ? P : empty;

declare function Poly<T>(props: {value: T}): void;

declare function inferThroughEval<T>(props: Props<typeof Poly<T>>): T;
const inferred = inferThroughEval({value: 42}); // :( underconstrained
inferred as number;
