//@flow strict

declare opaque type T: mixed;
declare var o: { p: T };

// guard
if (o.p) {
  (o.p: T); // OK
}

// refine
if (o.p != null) {
  (o.p: T); // OK
}


declare opaque type TT: mixed;

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
