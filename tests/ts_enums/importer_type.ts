// `import type` imports only the type (matches TS). Bare `Color` and member
// `Color.Red` both work as types; using the name as a *value* is an error
// (cf. TS1361 "cannot be used as a value because it was imported using
// 'import type'").
import type { Color } from "./exporter";

declare const x: Color; // bare enum name as a type
x satisfies number; // OK: members are assignable to their representation type

declare const m: Color.Red; // member used as a type
m satisfies 1; // OK

// A non-member literal is rejected, proving the imported bare type is the union
// of member literals (and not `any`), even through the `import type` path.
const xbad: Color = 99; // ERROR

const v = Color.Red; // ERROR: `Color` was imported with `import type`
