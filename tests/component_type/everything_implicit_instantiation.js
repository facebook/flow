declare function idComponent<Props: {...}, Renders: React.Node>(
    c: component(...Props) renders Renders,
): component(...Props) renders Renders;

declare component A();
declare component B(ref: React.RefSetter<Set<string>>, foo: string, bar: number) renders? A;

const C = idComponent(B);
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: number) renders? A; // ok
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: string); // error
C as component(ref: React.RefSetter<Set<number>>, foo: string, bar: number); // error
C as component(ref: React.RefSetter<Set<string>>, foo: string, bar: number) renders A; // error
