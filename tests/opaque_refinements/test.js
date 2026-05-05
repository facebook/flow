//@flow strict

declare opaque type T: unknown;
declare var o: { p: T };

// guard
if (o.p) {
  o.p as T; // OK
}

// refine
if (o.p != null) {
  o.p as T; // OK
}


declare opaque type TT: unknown;

declare let content:
| void
| null
| boolean
| number
| string
| TT;

if (content != null) {
  content = content ? 'TRUE' : 'FALSE'; // ok
}

{
  declare opaque type S;
  declare const x: ?S;

  if (x == null) {
    x as null | void; // ok
  }
}
