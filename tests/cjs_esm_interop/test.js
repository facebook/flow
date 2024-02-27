import * as CJS_NS from './cjs_value_type_implicit_naming_conflict';
CJS_NS.Foo; // ok: read of value

// Type exports will be forwarded to the CJS module type after `require(<module with type exports>)`
import type {Bar} from './cjs_require_default_exported_with_types';
declare var bar: Bar;
(bar: string); // error: number ~> string

// Spreading a namespace import with type exports will downgrade the namespace to the value part,
// erasing all type exports.
import type {T1, T2} from './cjs_spreading_type_exports'; // error
