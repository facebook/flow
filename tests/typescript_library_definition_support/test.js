// @flow
import { greet, version, helper } from 'my-lib';
import type { Config } from 'my-lib';
import { compute } from 'my-lib-js';
import { value } from 'my-lib-shadow';

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
