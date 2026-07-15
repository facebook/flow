// @flow

// `export *` must NOT forward the default, so this default import resolves to
// nothing and is an error (matches the ES spec and the fixed bundler runtime).
import DefaultThroughStar from './barrel';
DefaultThroughStar as unknown;

// Named exports are still forwarded via `export *`.
import {named} from './barrel';
named as number; // ok

// `export * as inner` builds a namespace object, which per spec still includes
// the source's default. This path is unaffected by the fix.
import {inner} from './ns_reexport';
inner.default as number; // ok, default is present on the namespace
inner.named as number; // ok
