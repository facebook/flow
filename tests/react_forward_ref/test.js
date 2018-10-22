import * as React from "react";

function F(props: { foo: string }, ref) { return null; }
const Fr = React.forwardRef(F);
<Fr />; // error: missing `foo`
<Fr foo={0} />; // error: number ~> string
<Fr foo="" />; // ok
Fr.displayName = "Fr"; // ok

// props subtyping is property-wise covariant
function G(props: { foo: string|number }) { return null; }
const Gr = React.forwardRef(G);
<Gr foo="" />; // ok

var Z = 0;
React.forwardRef(Z); // error, expected render function

