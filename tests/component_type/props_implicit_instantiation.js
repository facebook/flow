declare function idComponent<Props: {...}, Instance>(
    c: component(ref: React.RefSetter<Instance>, ...Props),
): component(ref: React.RefSetter<Instance>, ...Props);

declare component A(ref: React.RefSetter<Set<string>>, foo: string, bar: number);

const C = idComponent(A);
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: number); // ok
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: string); // error: string ~> number
C as component(ref: React.RefSetter<Set<number>>, foo: string, bar: number); // error: number ~> string
