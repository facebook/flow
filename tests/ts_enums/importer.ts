// Consuming a TS enum imported from another module (the .d.ts use case).
// A single `import` brings the name into both the value and type namespaces.
import { Color, Status } from "./exporter";

// Value use + member literal types survive the module boundary.
const r = Color.Red; // OK
r satisfies 1; // OK
Color.Green satisfies 2; // OK
Color.Red satisfies number; // OK

// Bare enum name as a type = union of member literals.
const c: Color = Color.Red; // OK
const c2: Color = 1; // OK: member value
const bad: Color = 99; // ERROR

// Member used as a type.
let m: Color.Red = Color.Red; // OK
let mbad: Color.Red = Color.Green; // ERROR

// String enum across the boundary.
const s: Status = Status.Active; // OK
Status.Active satisfies "active"; // OK
