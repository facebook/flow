declare function idComponent<Renders: React.Node>(
    c: component() renders Renders,
): component() renders Renders;

declare component A();
declare component A2();
declare component B() renders? A;

const C = idComponent(B);
C as component(); // ok: renders? A ~> default renders
C as component() renders? A; // ok
C as component() renders A; // error: renders A ~> renders? A
C as component() renders? A2; // error: renders? A2 ~> renders? A
