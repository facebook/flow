import { extractor } from "./exported_extractor";

declare function idComponent<Props: {...}>(
    c: component(...Props),
): component(...Props);

declare component A(ref: React.RefSetter<Set<string>>, foo: string, bar: number);

const C = idComponent(A);
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: number); // ok
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: string); // error: string ~> number
C as component(ref: React.RefSetter<Set<number>>, foo: string, bar: number); // error: number ~> string

declare component B(foo: string, bar: number);
const props1 = extractor(B); // ok
props1 as {+foo: number, +bar: string}; // error: string != number

declare function removeSomeProps<Props: {...}>(c: component(foo: string, ...Props)): Props;
const props2 = removeSomeProps(B); // ok
props2 as {+bar: number}; // ok
props2 as empty; // error
