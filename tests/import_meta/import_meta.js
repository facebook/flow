// @flow

// NOTE: The following tests all currently yield a "Not supported."
// error because Flow does not process MetaProperty expressions at all.
// The comments above each test case describe what the behavior _should_
// be once typechecking is implemented.

// You can access `import.meta` at the root of the module.
import.meta;

// You can write to `import.meta` at the root of the module.
import.meta.hello = "world";

// You can use `import.meta` in a normal expression context.
const meta = import.meta;

// All properties are `mixed`.
(meta.whatever: mixed);
(meta.whatnever: number); // error

// Properties are writable.
meta.writeable = "definitely";
