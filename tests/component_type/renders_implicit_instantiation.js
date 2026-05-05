declare function idComponent<Renders extends React.Node>(
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

declare function RendersAny(): any;
declare function extractRenders<R extends React.Node>(c: component(ref?: React.RefSetter<unknown>, ...empty) renders R): R;
const extractedAnyRender = extractRenders(RendersAny);
extractedAnyRender as empty; // ok: any ~> empty
