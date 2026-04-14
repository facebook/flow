// @flow
import { greet, version, helper } from 'my-lib';
import type { Config } from 'my-lib';
import { compute } from 'my-lib-js';
import { value } from 'my-lib-shadow';
import { foo } from 'pkg-with-types';
import { bar } from 'pkg-with-typings';
import { baz } from 'pkg-with-exports-types';
import { qux } from 'pkg-with-exports-and-types';
import { val } from 'pkg-with-types-extensionless';
import { nested } from 'pkg-with-nested-export-types';

// Extensionless and directory imports resolve
greet("world") as string; // OK
greet(42) as string; // ERROR — number not string
helper(1) as string; // OK
version as number; // OK

// .js → .d.ts mapping resolves
compute(1) as number; // OK
compute("x") as number; // ERROR — string not number

// Shadowing: .d.ts types win over .js
value as number; // OK
value as string; // ERROR — number not string (d.ts says number)

// Type import works
declare const cfg: Config; // OK
cfg.debug as boolean; // OK

// package.json "types" field: resolves to types.d.ts, not index.js
foo as number;
foo as string; // ERROR — number not string (types.d.ts says number)

// package.json "typings" field (legacy): resolves to types.d.ts
bar as number;
bar as string; // ERROR — number not string (types.d.ts says number)

// package.json "exports" with "types" condition: resolves to types.d.ts
baz as number;
baz as string; // ERROR — number not string (types.d.ts says number)

// package.json with both "exports" and "types": exports wins, "types" field ignored
qux as number;
qux as string; // ERROR — number not string (exports types.d.ts says number)

// package.json "types" field with extensionless target: resolves to .d.ts, not .js
val as number;
val as string; // ERROR — number not string (index.d.ts says number)

// package.json "exports" with "types" nested inside "import": resolves to types.d.ts
nested as number;
nested as string; // ERROR — number not string (types.d.ts says number)
