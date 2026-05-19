// @flow

// @types fallback: untyped-lib has no own types, resolved via @types/untyped-lib
import { untypedVal } from 'untyped-lib';
untypedVal as number; // OK
untypedVal as string; // ERROR — number not string (@types says number)

// @types subpath: subpath imports preserve the subpath under @types
import { subVal } from 'untyped-lib/sub';
subVal as boolean; // OK
subVal as string; // ERROR — boolean not string (@types/untyped-lib/sub says boolean)

// @types priority: my-lib has own .d.ts types, @types/my-lib should NOT be used
// greet returns string (from my-lib's own index.d.ts), not boolean (from @types/my-lib)
import { greet } from 'my-lib';
greet("world") as string; // OK — uses my-lib's own types
greet("world") as boolean; // ERROR — string not boolean

// @types with package.json: pkg-with-json has package.json pointing to JS, @types provides types
import { jsonVal } from 'pkg-with-json';
jsonVal as string; // OK
jsonVal as number; // ERROR — string not number (@types/pkg-with-json says string)

// @types scoped package: @scope/pkg resolves to @types/scope__pkg
import { scopedVal } from '@scope/pkg';
scopedVal as number; // OK
scopedVal as string; // ERROR — number not string (@types/scope__pkg says number)

// Explicit @types/* import: the @types fallback must NOT rewrite this to
// @types/types__missing. node_modules/@types/types__missing/ exists, but
// `@types/missing` is not present and resolution should fail instead of
// silently picking the wrong package.
import { trapVal } from '@types/missing'; // ERROR: cannot resolve module
trapVal as number; // unreachable — trapVal is `any` (resolution failed)

// react exclusion: `import 'react'` must always come from Flow's builtin lib
// defs, never @types/react. node_modules/@types/react/index.d.ts exports a
// sentinel that should NOT be reachable; if the exclusion ever breaks, this
// import would succeed and the cast would not error.
import { at_types_react_marker } from 'react'; // ERROR: not exported by react
at_types_react_marker as number; // unreachable — at_types_react_marker is `any`
