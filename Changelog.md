###v0.35.0

Likely to cause new Flow errors:
* Flow now knows that calling `addEventListener()` with `'click' and 'dblclick'` will pass a `MouseEvent` to the listener. This means you might need to update `foo.addEventListener('click', (e) => bar())` to `foo.addEventListener('click', (e: MouseEvent) => bar())`.

New Features:
* Better error messages in a bunch of situations
* flowtype.org/try now has an AST tab that shows the parsed AST of the example

Notable bug fixes:
* Bindings that come from imports are now treated as const
* Found and fixed a few situations where Flow was emitting redundant errors
* Some if statement refinements were sticking around after the if statement in some situations when they should not have.

Misc:
* Various libdef fixes and improvements
* Various docs fixes and improvements
* If `foo` has the typed `mixed`, we now allow `foo.bar` if you check that `foo` is not `null` or `undefined`.

Parser:
* Better error message if you try to make a class property optional (currently unsupported)
* Dropped support for `let` statements, which never made it into the spec (thanks [@andreypopp](https://github.com/andreypopp))

###v0.34.0

Likely to cause new Flow errors:
* Dictionary types (i.e. `{[key: string]: ValueType}`) were previously covariant which proved to be a significant source of unsoundness. Dictionary types are now invariant by default in order to fall into consistency with other collection types. It is possible to opt in to explicit covariance with new syntax: `{+[key: string]: ValueType}`, but note that this is now *enforced* covariance -- which means the dictionary can no longer be written into (only read from). For mutable collections, consider using `Map`/`Set`/etc. Please see this [blog post](https://flowtype.org/blog/2016/10/04/Property-Variance.html) for more information on variance.
* Object property types are now invariant by default. New syntax allows explicit opt-in to enforced covariance: `type T = {+covariantProp: string}`. Please see this [blog post](https://flowtype.org/blog/2016/10/04/Property-Variance.html) for more information on variance.
* Object *method* types are now covariant by default. So this: `type T = {covariantMethod(): string}` is the same as `type T = {+covariantMethod: () => string}`. Please see this [blog post](https://flowtype.org/blog/2016/10/04/Property-Variance.html) for more information on variance.

New features:
* New `empty` type annotation. This is the ["bottom type"](https://en.wikipedia.org/wiki/Bottom_type) which is the type that has no possible values. This is mostly useful for asserting impossible types right now (see [the commit description](https://github.com/facebook/flow/commit/c603505583993aa953904005f91c350f4b65d6bd) for more details).
* All server commands now have a `--quiet` flag to suppress server-status information that would otherwise be printed to stderr.
* It's now possible to specify an "@jsx" pragma to override the implicit default of `React.createElement`. See [the commit](https://github.com/facebook/flow/commit/d2930e135f9c34a89a226c2ca36eb4bcab7b59db) message for more details.
* [async iteration](https://github.com/tc39/proposal-async-iteration) is now Stage 3, so Flow now supports async generators and `for-await-of` statements.

Notable bug fixes:
* Calling `get-def` and `autocomplete` on specifiers in import statements now works properly and links in to where the specifier is exported in the other file.
* `Generator.prototype.return` now returns a possibly-unfinished `IteratorResult` object rather than a definitely-done iterator result. See #2589 for more details.
* Fixed an issue with inferring the proper return-type of iterators coming from a generator with multiple return-types. See #2475 for more details.
* Fixed an issue where a non-polymorphic class-instance used as a CommonJS export wasn't omitting underscore-prefixed members when the `munge_underscores` config option is set to `true`.
* Fixed an issue where Flow would previously not consider non-@flow files when re-calculating type dependencies on a change to the filesystem. This caused sporadic issues where Flow might think that a module is missing that actually is not! This is now fixed.

Misc:
* Significant memory usage optimizations by normalizing common aspects of the many "reason" structures stored in memory to use ocaml variants.
* Significant memory usage optimization by compressing the contents of the shared heap used by the persistent server.
* Parser now allows for duplicate properties and accessors in objects per the latest ES spec.
* Flow parser is now tested against esprima3 tests
* `yield` expressions no longer evaluate to an optional type. This is unsound, but the inconvenience was so prevalent that we decided to relax the issue for now. See #2080 for more details.
* Various core libdef updates.

Parser breaking changes:
* Updated the parser to use the latest `ExportNamedDeclaration` and `ExportDefaultDeclaration` nodes from ESTree rather than the outdated `ExportDeclaration` node.
* `export default class {}` now correctly emits a `ClassDeclaration` rather than `ClassExpression` per [this estree issue](https://github.com/estree/estree/issues/98).
* Updated `ExportSpecifier` property names from `id` -> `local` and `name` -> `exported` per the latest ESTree spec.
* Update `ExportBatchSpecifier` to a `ExportNamespaceSpecifier` node per the latest ESTree spec.
* Renamed `SpreadElementPattern` -> `RestElement` per the latest ESTree spec.
* Node-properties of `ObjectPattern` are now named `Property` and `RestProperty` per the latest ESTree spec.
* Use a `Super` node now instead of an `Identifier` node per the latest ESTree spec.
* `{ImportNamedSpecifier` and `ImportDefaultSpecifier` nodes now use proper `local` and `remote` property names, per the latest ESTree spec.
* The `mixed` type annotation is now represented with a special `MixedTypeAnnotation` node (same as Babylon has been for a while).
* The `NullTypeAnnotation` annotation node is now called `NullLiteralTypeAnnotation` in order to match Babylon.

###v0.33.0

Likely to cause new Flow errors:
* Its now an error to add `mixed` to `number`

New features:
* `suppress_comment` now defaults to matching `// $FlowFixMe` if there are no suppress_comments listed in a .flowconfig
* The Flow server no longer restarts itself when a `package.json` file is changed
* The Flow server no longer restarts itself if a libdef file is touched, but not actually changed
* Added support for using Iterables with `Promise.all()` (thanks @vkurchatkin!)

Notable bug fixes:
* Fixed an issue where some predicates could cause Flow to crash
* Fixed an issue where we weren't propertly looking things up on `Function.prototype` and `Object.prototype`
* Fixed an issue where Flow could crash when extracting coverage on empty types
* Fixed an issue where long paths that are ignored could give a bunch of warnings on Windows
* Fixed an issue where `flow get-def` wouldn't hop to the location of a type coming through an `import type`
* Fixed an issue with dictionary types where using an `any`-typed variable as a computed-property lookup results in the wrong property-value type
* Fixed some issues where Flow wouldn't allow defininition of properties or methods called "static" on classes
* Fixed an issue where Flow wouldn't permit `throw`s at the toplevel of a module
* Fixed an issue where adding a file to `[libs]` with an extension not listed in `module.file_exts`, it would previously be silently ignored
* Fixed an issue where `import * as` on a `declare module.exports: any;` libdef would not result in a module with every possible named export
* Fixed a parsing issue where `"JSX attributes must only be assigned a non-empty expression"` syntax errors sometimes point to the wrong line
* Fixed parsing of setter methods with destructured parameters
* Fixed a parsing issue where it wasn't possible to use `get` or `set` in object short notation
* Fixed a parsing issue where we previously weren't allowing strings in async method names: `x = { async 123() { await y; } }`
* Fixed an issue where the parser previously wouldn't recognize the `u` regex flag

Misc:
* Various built-in libdef improvements
* Various significant shared memory optimizations
* When a package.json key is read by Flow during module resolution, don't wrap it in quotes
* Added parser (but not yet typechecker) support for `new.target`
* Added `--pretty` flag to all commands that have a `--json` flag
* Flow now prints an exception instead of segfaulting if there is a heap overflow
* Only print JSON for `flow coverage` when `--json` is passed (thanks @aackerman!)

Parser breaking changes:
* Removed 'lexical' property from `SwitchStatement`
* Removed `guardedHandlers` from `TryStatement`
* Use `AssignmentPattern` for function param defaults to match ESTree
* Use `RestElement` for function rest params to match ESTree
* Fixed the location info for `ExpressionStatement`
* Fixed the location info for `CallExpression` and `MemberExpression`

###v0.32.1

Notable bug fixes:
* If Flow runs out of heap space, it now throws an exception instead of segfaulting.

###v0.32.0

Likely to cause new Flow errors:
* If you check that an object `obj` has a property `foo` that Flow doesn't know about, Flow will now refine `obj.foo` to the type `mixed`. If you then try to use `obj.foo` in an unsafe way (like as a function), you might start seeing errors mentioning `` property `foo` of unknown type ``. The fix is to either fix the type of `obj` to include an optional property `foo`, or to rewrite code like `obj.foo && obj.foo(x)` to `typeof obj.foo === "function" && obj.foo(x)`
* We've fixed the type of `Array.prototype.find` to reflect the fact that it can return `undefined`
* We've tightened up the checking of tuple types


New Features:
* New syntax for exact object types: use `{|` and `|}` instead of `{` and `}`. Where `{x: string}` contains at least the property `x`, `{| x: string |}` contains ONLY the property `x`.
* Flow now watches for `.css`, `.jpg`, `.png`, `.gif`, `.eot`, `.svg`, `.ttf`, `.woff`, `.woff2`, `.mp4` and `.webm` files (this list is not configurable at the moment). We call them resource files. If you require a resource file, you get a `string` (except for `.css` files, for which you get an `Object`). You should still be able to use `module.name_mapper` to map a resource file to a mock, if you prefer.
* We're starting work on `flow gen-flow-files`, which consumes Flow code and outputs `.flow` files containing only the types exported. It's alpha-level and we're still iterating on it, so use at your own peril!

Notable bug fixes:
* Fixed a bug where, if a module has a CommonJS export of type `any`, then the types it exported were also given type `any`
* v0.31.0 contained a regression where arrays and promises required more annotations than they should. This is fixed.
* Fixed use of tagged unions with intersections and generics.
* Jump to definition for overridden methods was jumping to the wrong method. This is now fixed.
* `flow coverage` had a non-termination bug. This is now fixed.

Misc:
* Lots of improvements to the builtin libdefs! Thanks for all the pull requests!

###v0.31.0

Likely to cause new Flow errors:
- Fixed an issue where an `any` type could actually prevent errors from showing up in places that actually should surface errors
- `Array.isArray()` now refines arrays to `Array<mixed>` instead of `Array<any>`

New Features:
- When the `munge_underscores` option is set, Flow no longer requires annotations on exported classes with munged methods. (thanks [@pvolok](https://github.com/pvolok)!)
- Added a new "magic" type called `$PropertyType<T, 'x'>`. This utility extracts the type of the property 'x' off of the type T.
- It is now possible to use leading-`|` and `&` on any type annotation, not just in type aliases

Notable bug fixes:
- Signficant perf improvements on checking disjoint unions
- Fixed an issue where `flow start` would sometimes hang on Windows
- Fixed an issue where `flow get-def` on a react element would always point to the internal react libdef (rather than the component that defines the element)
- Error messages for builtin types are now more descriptive in more scenarios
- Fixed the order in which destructured function params were processed. This previously caused issues with variable references within the destructuring
- Error messages for union type errors now point to more helpful locations in some cases
- Fixed a long-standing confusing error message blaming property accesses on the "global object" (when the global object isn't actually involved at all)
- Improved error message when trying to import a named export from a module that only has a default export
- Fixed an issue where `Promise.resolve(undefined)` would not always error as it should
- Fixed an issue where async functions that return void do not properly enforce their return type
- Fixed an issue where aliased object types may not error when an invalid property is accessed on them
- Fix `flow coverage` when reporting coverage on empty files
- Printed types in some circumstances (autocomplete, type-at-pos, etc) are now capped on size to prevent overflows in tools that consume them. When the size overflows the cap, `...` will be printed as an overflow placeholder
- Various built-in libdef updates

###v0.30.0

Likely to cause new Flow errors:
- Fixed `React.PureComponent`'s definition, so previously missed errors are now reported
- The definition of `console` in the build-in libdef has been filled out and is no longer `any`.

New Features:
- From now on we're going to start publishing Windows builds with each release. Please report any issues you have!
- Forward references in type annotations: you can now reference a class in a type annotation before the class is declared
- T is now covariant in `Class<T>`. So if class `B` extends class `A`, then `Class<B>` is now a subtype of `Class<A>`
- Flow now lets you destructure objects with computed keys.
- `flow check-contents --respect-pragma` - `check-contents` checks whatever you give it, regardless of `@flow` or `@noflow`. This option changes the behavior to respect the pragma.
- `flow check-contents` will now report parsing errors too (thanks [@nmote](https://github.com/nmote)!)

Notable bug fixes:
- Fixed `--trace` behavior when we unify types
- Fixed situation where the client could spin-wait for the server
- Fixed non-termination triggered by certain calls to `Function.prototype.{apply,call}`
- Fixed issue where "Duplicate module provider" errors would stick around even after being fixed.
- (Windows) Fixed how the flow client tails the flow server's logs
- (Windows) Fixed the log rotating
- (Windows) Fixed issues where Flow would report Process Handles instead of Process IDs, leading to bad messages and a non-functional `flow stop`
- (Windows) Fixed build outside of a git or hg repository
- (Windows) Better error messages when you have paths that are too long

Misc:
- `undefined` is now just a global variable declared in the libdefs
- Various built-in libdef improvements
- Nifty PR from [@nmn](https://github.com/nmn) which teaches flow that `nullableArr.filter(Boolean)` is non-nullable

###v0.29.0

New features:
- Tagged unions of interfaces should now work in the same way that tagged unions of type aliases work.

Notable bug fixes:
- Building Flow outside of a git or hg repo should work now
- If you declare an overloaded function with one overload that handles a `string` argument and one overload that handles a `number` overload, then passing in a union `string | number` should now work.
- Annotations for destructured patterns now work like non-destructured patterns. Types no longer flow through these annotations, rather they are checked against the annotations and then the annotations are used instead.
- Allow `import {type} from "Foo"`
- We had a bug where Flow wouldn't complain about duplicate haste modules. This is now fixed
- Fix for edge case hit when using `experimental.strict_type_args=false` and type parameter upper bounds.

Misc:
- Various built-in libdef improvements
- Some error location fixes
- Lots of Windows support work. Should build and mostly work, but not fully stable yet.
- Some performance wins
- `flow check --json --profile` now includes profiling info in the JSON
  response

###v0.28.0

Likely to cause new Flow errors:
- [Significant fix](https://github.com/facebook/flow/commit/2df7671e7bda770b95e6b1eaede96d7a8ab1f2ac) to matching members of union types. This fix very likely surfaces typechecking issues that were missed before. [Check out the blog post for more details](https://flowtype.org/blog/2016/07/01/New-Unions-Intersections.html)
- Variables in template literals are now subject to the same rules as variables being concatenated with a string using `+`. This may surface template strings with variables that may not safely coerce to a string.
- Type arguments are now required to be specified for type annotations that have type arguments. Previously they were implicitly filled in as `any` -- but now this must be explicit. This behavior is toggleable via `experimental.strict_type_args=false`, but this config option will be removed in a near-term future release. [There is also a codemod to help you automatically update old code.](https://github.com/flowtype/flow-codemod/blob/master/transforms/strict-type-args/README.md)

New Features:
- Support for `declare export` within `declare module` bodies. When `declare export` is used, the module being declared is inferred as an ES module. This enables the ability to export both named and default exports using `declare module`.

Notable bug fixes:
- Fixed a bug on OSX where Flow might crash in rare circumstances
- Fixed an issue where automatic semicolon insertion for class properties [wasn't behaving properly in some circumstances](https://github.com/facebook/flow/issues/1682).
- It is no longer required to annotate a property on an exported class if an initializer value is present. Instead, the initializer value itself can be directly annotated. This makes annotation of class properties that are functions much less cumbersome.
- Fixed a race-condition where Flow may not properly error when it encounters two modules with the same name (using the "haste" module system)

Misc:
- Flow now looks for `@flow` in all comments at the top of the file (rather than just the first). It's also possible to configure this using a new `max_header_tokens` .flowconfig option.
- Various built-in libdef improvements
- Performance improvements for the `flow coverage` command

###v0.27.0

Notable bug fixes:
- Fixed a filesystem race condition where Flow would note that a directory exists just before it is deleted, then try to read the directory
- Fixed some issues with disjoint unions that have complex definitions
- Fixed an issue where functions that come after a return aren't analyzed (even though they are hoisted)
- A few updates to the Node.js library definitions
- Fixed a race condition when adding specific properties to dictionary types
- Various performance improvements
- `--strip-root` is now applied to the output of `--verbose`
- Fixed an issue where duplicate method declarations weren't understood correctly

###v0.26.0
([@gabelevi](https://github.com/gabelevi) mistakingly listed a few v0.26.0 changes as being in v0.25.0. The Changelog has been updated to reflect reality. Sorry!)

Likely to cause new Flow errors:
- Flow now understands stateless functional React components, which may reveal many errors if you use them heavily.

New Features:
- Support for stateless functional React components!
- Support for the exponentiation operator (`**`)
- New `flow ls` command to list the files that Flow can see.

Notable bug fixes:
- Fixed parsing of `/x/* 5 */y/` - that's no comment!
- Fixed parsing of newlines or comments at the end of a template literal.
- Fixed an incremental bug where Flow didn't notice new JSON files in certain situations
- Fixed a parsing service crash when a file appears and disappears suddenly.

Misc:
- Restored a bunch of deprecated/experimental/browser-specific APIs to the builtin flowlib and made them optional
- Fixed up parser tests and further integrated them into CI.
- Lots of refactoring!

###v0.25.0
Likely to cause new Flow errors:
- [@marudor](https://github.com/marudor) made a tremendous effort to clean up the builtin flowlib definitions, adding missing things, fixing annotations, and removing non-standard and deprecated features. If you're relying on these things, then you may have new errors.
- In the past, generic types could leave off the type arguments. Flow is moving towards making these required. Applying type arguments to a polymorphic type (e.g. `Map<string, number>`) is like calling a function. If writing `my_function` was the same thing as writing `my_function()`, it would be really difficult to pass functions as values. Similarly, by making type arguments required, it frees us up to do more with our polymorphic types. If you have a polymorphic type with default types, like `type Foo<T = number>`, you can now write `Foo<>` to apply 0 type arguments to the polymorphic type `Foo`.

  To ease migration, v0.25.0 still allows you to omit type arguments by default. v0.27.0 will start enforcing that type arguments are always supplied. To enable this behavior in v0.25.0, you can add `experimental.strict_type_args=true` to the `.flowconfig`.

New Features:
- `declare module.exports: type;` <-- this is the new syntax to declare the CommonJS export type. Previously, you had to write `declare var exports: type;`. This can be used in `.flow` files and in `declare module` declarations.

Misc:
- Now Flow builds on OCaml 4.03.0
- Fixed up the parser tests (thanks for the help [@marudor](https://github.com/marudor)!) and have started running those in CI
- A ton of refactoring and clean up

###v0.24.2

- Fixed a bug where Flow might run out of memory in a repository with a lot of non-flow files

###v0.24.1

- Fixed a bug where `autocomplete` can show internal variable names for files that use destructuring

###v0.24.0

New features:
- Many common errors now have more contextual error messages. [Check out the test file changes](https://github.com/facebook/flow/commit/7b8c3aed5d852ad9b8290076508658168d0d5fde#diff-839cf9ce7d26ef86255e179b8a539fc7) to see what this looks like!
- If a `<PROJECT_ROOT>/flow-typed/` directory exists, Flow will now assume it is a [libs] directory by default to help reduce the amount of out-of-the-box configuration that is necessary. (Thanks @splodingsocks!)
- Support for specifying a default on a type parameter declaration. For example: `type Iterator<Yield, Return=void, Next=void> = ...`.
  - NOTE: [The pull request to add syntax support for this is pending](https://github.com/babel/babylon/pull/25), so it may be necessary to wait on that to ship before using this feature

Notable bug fixes:
- Fixed the `flow coverage` command.
- Fixed crashes when running `type-at-pos` and `dump-types` over destructuring.
- Fixed a refinement bug when evaluating a method call on a refined variable in some cases.
- Fixed an issue where some system commands could throw if Flow attempts to read a directory that it doesn't have permission to read.
- Fixed a bug where the inferred return type of some functions that *might* return `void` could end up as *exactly* `void` (rather than a union of the possible return types).
- Fixed a bug where the server would crash if a `.json` file is deleted after a server was already running.
- Type applications that don't have the right number of type parameters (i.e. `Map<number>`) now emit a more clear error.
- Lots of dom/bom improvements sent in from contributors! (big thanks to @marudor for lots of these!)

Misc:
- Better locations for logical operator errors
- Better error message sorting:
  - Sorts errors in `.json` files by name coalesced with `.js` files
  - Sorts all internal errors before parse errors before type/inference errors
  - Sorts lib file errors before source file errors
- Perf improvements for some comparisons of polymorphic types

###v0.23.1

- Fixed parsing of JSON files with duplicate object keys

###v0.23.0

Likely to cause new Flow errors:
- When you refine a `mixed` variable with `typeof myVar === 'object'`, we used to refine the type of `myVar` to `null | Object`. Now it is refined to `null | {[key: string]: mixed}`. This means `myVar.prop` has the type `mixed` rather than `any`.
- Removed non-standard Promise methods. Previous versions of Flow specified the type of Promise.prototype.done and Promise.cast, which are not standard or implemented in browsers. If you rely on a polyfill that does provide these methods, you can redeclare the Promise class in your project's local libs folder. Note that you need to copy the entire class declaration from lib/core.js in order to add methods and properties. It's not currently possible to extend the builtin declarations, but it is possible to redefine them.

New features:
- Errors involving union or intersection types now include more information about why the various branches failed
- `flow init` now has more command line flags to specify what should be in the created `.flowconfig`
- `flow ast` now can parse JSON files
- Comments are now supported in `.flowconfig` files. Lines starting with `#` or `;` are ignored
- In the `[ignore]` section of a `.flowconfig` you can now ignore a relative path with `<PROJECT_ROOT>/path/to/ignored`
- Most `flow` commands have an `--ignore-version` flag to skip the version check specified in the `.flowconfig`.
- Added a `module.use_strict` option to the `.flowconfig`. When it is true, Flow will treat every file as if it has the `"use strict";` directive.
- Using strict equality, you can now refine the `number` type into number literal types. (e.g. after `x === 0` Flow now knows that x has the type `0`)
- Flow no longer requires return type annotations for exported functions if Flow can fully infer the type.
- `flow force-recheck FILE1 FILE2` command tells the flow server that `FILE1` and `FILE2` have changed and that it should recheck. This is intended for tooling that might be racing the file system notifications.
- Flow is smarter about what `!x` evaluates to for various types of `x`.
- `<Foo />` is now allowed when `Foo` is a `string`. If `$JSXIntrinsics` is defined, `Foo` must be a subtype of `$Keys<$JSXIntrinsics>`
- Support for class decorators in addition to property decorators (also gated behind the `esproposal.decorators` config option). Thanks @marudor!

Notable bug fixes:
- Disallow `(obj: SomeClass)` except for when `obj instanceof SomeClass`
- Fixed setting temp dir via the `.flowconfig`
- Added missing `all` flag to the `.flowconfig`
- Fixed a bug when destructuring a non-literal object type using a pattern with defaults
- Fixed the `--strip-root` flag for displaying errors
- Classes can now have properties named `async`
- Fixed refinements like `if (foo[0]) { ... }`, which should work like `if (foo["prop"]) { ... }` does. That is, Flow should remember that `foo[0]` exists and is truthy.
- Fixed parsing docblocks with CRLF line endings
- Fixed autocomplete within if statements

Misc:
- Added more info and structure to JSON output, without removing or existing fields
- Loads of improvements to the builtin libraries
- Bunch of perf improvements
- Clarified some errors messages and error locations
- `flow start --json` will start a server and output a JSON blob with info about the new server
- Slowly improving tracking side effects in switch statements
- Some improvements to pretty printing errors
- `Object.values()` and `Object.entries` return `Array<mixed>` and `Array<[string, mixed]>` respectively, since Flow currently is never sure that it knows about every property in an object.

###v0.22.1

- Patch release to fix some JSON parsing issues that went out in v0.22.0

###v0.22.0

Likely to cause new Flow errors:
- Overhaul of Flow's understanding of React APIs. Some of these updates remove old/deprecated React APIs. Check out the [new React docs](http://flowtype.org/docs/react.html) for an overview of how things work.

New features:
- Flow now gives precedence to library definitions over non-@flow implementation files. This means that it should no longer be necessary to specify a `node_modules` dependency in the `[ignore]` section of your `.flowconfig` if you have a library definition defined for that dependency.
- Significant improvements to `Promise.all`: We now preserve the type of each item in the array passed to `Promise.all()` so that it may be propogated through to the resulting `.then()` handler.
- We no longer try to parse files that are not marked with an `@flow` pragma. We anticipate this will improve performance for projects with large, non-Flow node_modules directories.
- Classes with static members are now subtype-compatible with structural object types
- It is now possible to specify a leading `|` or `&` for type aliases of long unions/intersections. This is useful, as one example, for disjoint unions with a large number of members (where each member sits on a new line):

```javascript
type MyDisjointUnion =
  | {type: 'TypeOne', ...}
  | {type: 'TypeTwo', ...}
  | {type: 'TypeThree', ...}
  ...
;
```

Bug fixes:
- Fixed an issue where an intersection of two object types did not always properly combine to match objects with members on both sides (https://github.com/facebook/flow/issues/1327)
- Fixed an issue where an object of some intersection type could not be used with the spread operator (https://github.com/facebook/flow/issues/1329)
- Fixed an issue where refinement-testing on an object of an intersection type wouldn't always work (https://github.com/facebook/flow/issues/1366)
- Fixed an issue where an intersection of function types with a common return type should type check against function types with union of param types
- Fixed an issue where refining `obj['abc']` didn't behave quite the same as refining `obj.abc`
- Fixed an issue where usage of `flow get-def` on the left-hand side of a `require()` wouldn't hop through the `require()` and to the actual location of the definition
- Fixed an issue where Flow would not give a clear error when trying to use `import type` to get a non-type export
- Fixed an issue `flow dump-types --json` was not as robust as it could be against outputting valid JSON in the event of certain kinds of errors
- Fixed an issue where Flow would not give a parse error if multiple ES exports with the same name are exported from a single ES module
- `declare class` declarations now properly define a built-in `name` property (like real class declarations do)

###v0.21.0

Likely to cause new Flow errors:
- ES6 react classes without state should now `extends React.Component<DefaultProps, Props, void>` (previously it was `extends React.Component<DefaultProps, Props, {}>)`
- ES6 react classes with state should declare `state: State;`
- Before, it was possible to test for properties in objects which did not exist (`if (o.noSuchP === foo) { ... }`). These kinds of checks will now cause a type error.

New features:
- Autocomplete for jsx properties
- [Typed JSX intrinsics](https://github.com/facebook/flow/commit/e0e44d392d6fa2bff36ea6aee87f965c66ee5b7e). This means you can list which jsx intrinsics exist (like `div`, `span`, etc) and specify which properties they have.
- Syntax for declaring variance at definition. For example, `interface Generator<+Yield,+Return,-Next> {...}`. Still pending transpiler support though.
- Refining `string` and union types with string equality now properly refines the types.
- Support for `export * as` from @leebyron's [Stage1 proposal](https://github.com/leebyron/ecmascript-more-export-from). Babel support [here](http://babeljs.io/docs/plugins/transform-export-extensions/)

Notable bug fixes:
- Fixed bug with class expressions due to `this` type
- Fixed autocomplete for `this`
- Recognizes exhaustiveness in `switch` statements with `default` case.
- Fixed "Did not expect BoundT" errors
- Fixed infinite loop in certain recursive types
- Fixed an incremental mode issue with deleted files
- Fixed an incorrect refinement when assigning an object to a variable.

Misc:
- Some internal errors now will be made user visibile instead of silently failing. They generally mean that Flow has some bug or is making an untrue assumption/assertion. If you see these please report them!
- Improvements to how we report certain types (type application, optional types) via our APIs
- Various sentinel improvements, including boolean sentinels
- Various improvements to the buildin flow libraries (thanks everyone for the pull requests!)

###v0.20.1

Bug fixes:
- Ironed out some issues with the `this` type

Misc:
- find package.json using normal parsing phase

###v0.20.0

New features:
- Initial support for a `this` return type for class methods
- Big improvements on error messages for unions and intersections
- `import typeof * as` now allows for concise access to the type of the ModuleNamespace object
- Flow now understands how to `require()`/`import` a .json file

Bug fixes:
- Fixed an issue where nested unions and intersections might not typecheck as expected
- Fixed issue where `declare type` wouldn't work in a `declare module` that has an `exports` entry
- Fixed an issue where the error formatter could fatal in some rare cases
- Fixed a bug where `Function.prototype.bind` would lose the types of the params on the function it output
- Fixed some variance bugs in the built-in Array.prototype library definitions
- Fixed a bug where using a list that doesn't contain ".js" in `module.file_ext` would cause internal flow libs to be ignored
- Fixed autocomplete to work better with general `Function` types
- Fixed some issues with const refinement
- Fixed various issues with `export * from` (it should work fully now)
- Fixed a bug where Flow might crash when an export has a wildcard typeparam

Misc:
- Some improvements to DOM and Node libdefs
- Various error position relevancy improvements
- Significantly improved general understanding of special functions like `Function.prototype.{bind,call,apply}`
- Improved error messages for `import` statements where the remote exports don't exist (or may be typo'd)
- Improvements to understanding of deferred initialization of `let` variables
- `flow get-def` will now hop through lvalues in variable assignments for more fine-grained "hop-tracing" of a variable back to its definition
- Objects with a `callable` signature can now be passed in to type positions that expect a function with a matching signature
- Significant improvements to efficiency/perf when recalculating types based on a change to a file with an already-running Flow server

###v0.19.0

Likely to cause new Flow errors:
- Flow syntax is now disallowed in non-`@flow` files. Use `@noflow` to work around this
- `import type * as Foo` is now disallowed in favor of `import type Foo`
- `require()` can only take a string literal
- ES6 react classes without defaultProps should now `extends React.Component<void, Props, State>` (previously it was `extends React.Component<{}, Props, State>)`
- ES6 react classes with defaultProps should declare `static defaultProps: DefaultProps;`
- Flow notices errors it missed before in `React.createClass()` react components
- Flow is now stricter about using uninitialized variables
- Stricter checking of `in` keyword

New Features:
- `flow coverage` command
- `null` type annotation
- Support for class field initializers, gated by `.flowconfig` options
- You can now override flowlib definitions with local lib files
- Basic support for computed properties
- Declaration files (.flow files). Long story short, if `foo.js` and `foo.js.flow` exist, Flow will prefer the latter and ignore the former.
- `declare export` - a way to declare the exported types in a non-lib file
- Array rest destructuring assignment support

Notable Bug Fixes:
- Fix "package not found" error in some symlink situations
- Object indexer should not imply callable signature
- Default param values can reference earlier params
- Fixed a case where we weren't substituting type parameters properly
- Fixed a situation where Flow would prefer an unchecked module over a library definition

Misc:
- Add `--root` arg to most client commands
- More repositioning of error locations
- `flow get-def`: Jump to module named in import statement
- Lots of fixes to make flow commands smarter about connecting to the server
- Smarter refinement in a bunch of situations
- freeze imports on all modules, and require() on ES6 modules
- You can now spread classes, like `var {x} = new Foo()`
- Interfaces now can be callable
- If you've refined an object property, that refinement survives access through a destructured refinement
- Better autocomplete results for primitives, objects, functions and unions
- `flow server` will write to log file in addition to stdout/stderr

###v0.18.1

Likely to cause new Flow errors:

- Flow is now stricter (and more consistent) about how `null` works when used as an initialization value for object properties that are mutated later. So `let o = {prop: null}; o.prop = 42;` is now an error and requires that `null` be annotated: `let o = {prop: (null: ?number)};`.
- The return type of RegExp.prototype.match() is now properly annotated to return a nullable.

New Features:

- We now take advantage of the guarantee that `const` variables are read-only when doing refinements. This means that refinements of `const` variables now have much fewer caveats than refinements of `let`s or `var`s. For instance, it's safe to depend on the refined type of a `const` within a local function created in scope of the refinement, even if the function escapes.
- We now track which external variables a function actually modifies, and forget refinements to only those variables when a function gets called.
- New config options: `esproposal.class_static_fields=warn|ignore` and `esproposal.class_instance_fields=warn|ignore`. This allows the new [ES class fields](https://github.com/jeffmo/es-class-static-properties-and-fields) syntax to be ignored.
- New config option: `module.system.node.resolve_dirname`. This allows configuration of the name of the `node_modules` directory (or directories) used by the node module system. This is similar in behavior to webpack's [resolve.moduleDirectories](https://webpack.github.io/docs/configuration.html#resolve-modulesdirectories) config option.
- Added support for a `<PROJECT_ROOT>` token in the template string for the `module.name_mapper` config option. This token will be replaced with the absolute path to the current project root before mapping the module name. For example: `module.name_mapper='^\(.*\)$' -> '<PROJECT_ROOT>/src/\1'` would canonicalize an import from `"foo/bar/baz"` to `"/path/to/root/of/project/src/foo/bar/baz"`.


Notable Bug Fixes:

- Fixed a bug where we were enforcing TDZ on reads to let/const, but not on writes.
- Fixed a bug where any refinement clearing that occurred as a result of a function called in an if-statement was forgotten, meaning that lots of refinements were incorrectly allowed to stand.
- Fixed a bug where generator code couldn't previously call `yield` without an argument.
- Several improvements to generators.
- Various improvements and bug fixes for TDZ enforcement.
- Various other small improvements to refinements.
- Fixed an issue where errors could sometimes spew to stdout rather than stderr.

Misc:

- Removed `flow single` as it is effectively redundant with `flow check --max-workers=1`.
- The "server starting" output now gives more insight into progress.
- `flow --version` is now deprecated in favor of `flow version`.

###v0.17.0

New Features:

- New default error message format shows the code inline (thanks [@frantic](https://github.com/frantic)!)
- Autocomplete will keep trying even if the file fails to parse
- You can configure which file extensions Flow looks for (thanks [@eyyub](https://github.com/eyyub)!)
- Support for negative number literal types.

Notable Bug Fixes:

- Incremental `make` on OSX now works when `lib/` files change
- Fixed some issues around const destructuring
- Fixed some issues around destructuring in for-of and for-in loops
- Some emacs plugin fixes
- Object spreads are now handled in the correct order
- Generator `return()` and `throw()` methods are now supported
- Object types now allow keywords as the object keys (thanks [@samwgoldman](https://github.com/samwgoldman)!)
- importing & exporting `default` using named specifiers is now supported
- `Flow` now understands `this` in class static methods and `this.constructor` in class instance methods (thanks [@popham](https://github.com/popham)!)
- Fixed bug with array spreads
- Understand that all classes have a static `name` property

Misc:

- Improved `flow find-module`
- More error location improvements
- `Object` can now be called as a function to cast things to objects
- We've tried to standardize the error codes with which Flow exits. Some exit codes have changed, but the ones you probably use should be the same. At the moment they're [only documented in the code](https://github.com/facebook/flow/blob/b352b4c41283c1cb109ee2e8f6ef604ad4ac381b/src/common/flowExitStatus.ml#L63-L86)
- Flow understands the value of a negated number literal

###v0.16.0

Likely to cause new Flow errors:

- Some module exports that didn't require annotations before may now require annotations

New Features:

- Let/const support! Finally! (Huge props to [@samwgoldman](https://github.com/samwgoldman)...again :] )
- Support for `mixins` on `declare class` library definitions

Notable Bug Fixes:

- Improvements to types inferred from switch cases that fall through to a default cause
- Further improvements on symlink support

Misc:

- *Significant* performance improvements (both for initial start-up time and running re-calculation time)
- Improved `--traces` output

###v0.15.0

Likely to cause new Flow errors:

- Flow now treats class identifiers as being let bound. You cannot refer to a class before it is defined.
- If you accidentally use a tuple type instead of an array type for a rest param then Flow will complain
- You cannot use `this` before `super()` in a derived constructor, per ES6 semantics
- Our dictionary property (aka indexer property) support is much more robust and catches things it previously missed.
- We weren't properly enforcing optional properties in `interfaces` and `declare class`. Now we are.

New Features:

- Generators support, courtesy of [@samwgoldman](https://github.com/samwgoldman)
- # of worker processers is now configurable, defaults to the # of CPUs
- If Flow knows the value of a boolean expression, then it will know the value of that expression negated.
- Flow can remember refinements for things like `if(x.y[a.b])`
- `export type {type1, type2}` syntax

Notable Bug Fixes:

- Fixed issue where Flow would still read package.json for [ignore]'d path
- Fixed some leaky annotations that let data flow through them
- Fixed instance and class types to be considered truthy
- Flow still initializing message now fits in 80 chars, compliments of [@spicyj](https://github.com/spicyj)
- No longer will report hoisted declarations as unreachable code
- Fixed issue with how Flow chooses its tmp dir
- An async function can return a `T` or a `Promise<T>` and it means the same thing
- Fixed Flow forgetting about optional properties after an assignment refinement
- Fixed parser issue around future reserved keywords
- Optional parameters and rest parameters now work better together

Misc:

- Updated error locations. We've spent a lot of time auditing our error locations and trying to move them closer to the actual errors.
- esproposal.decorators option to tell Flow to parse and ignore decorators
- Bunch of updates to the libraries
- Some perf work
- Test output is colorized

###v0.14.0

Likely to cause new Flow errors:

- Assignment now triggers a refinement. If you have a variable that is a `?string` and you assign 'hello' to it, Flow refines its type to 'string'.

Likely to fix old Flow errors:

- We now treat missing type parameters as `any`. For example, previously `ReactElement` was treated as `ReactElement<*, *, *>`. Now it's treated as `ReactElement<any, any, any>`.

Misc:

- Basic unsafe support for getters & setters, gated behind the config option `unsafe.enable_getters_and_setters`
- Support for block comments inside of Flow's comment syntax
- Disable by default munging of class property names that start with an underscore, with an option to enable it
- Lots of small internal fixes and merged PRs
- Basic semver support for the .flowconfig version
- Support for `declare type` in lib files
- Type annotations are now opaque - other types will not flow through them
- You can configure the tmp dir that Flow uses

###v0.13.1

Likely to cause new Flow errors:

- Restricted `+` and `+=` to only allow strings and numbers, and no longer implicitly cast objects, booleans, null or undefined. Use `String(x)` to explicitly cast these values.
- Fixed a few bugs where types shared between modules may have lost precision or weren't enforced.

Misc:

- Added `import typeof` feature that allows you to import the type of a *value* export from another module. It is sugar for: `import MyThing_tmp from "MyModule"; type MyThing = typeof MyThing_tmp;` (except it removes the need for the intermediate `MyThing_tmp` variable)
- Added `flow ast` command to print a serialized JSON [ESTree](https://github.com/estree/estree) AST. (Note that this AST does not include types, just syntactic structure for now)
- Added support for class expressions
- Added support for following symlinks
- Added support for number-literal and boolean-literal annotations. (useful for things like enum types and refinements based on tests of equivalence between two variables)
- Added support for ES6 binary and octal integer literals
- Added support for `export type` within a CommonJS module that also uses the `module.exports = ...` pattern
- Added support for refining some union types down to their disjoint members
- Added support for covariant Promise type parameters
- Added improved support for understanding ES5-style imperative class definitions (i.e. via functions + prototypes)
- Fixed passing `undefined` to optional parameters
- Fixed return-type tracking for tagged template usage
- Fixed an issue where library parse errors would cause the flow server to continuously restart upon initialization without giving an error

###v0.12.0

Likely to cause new Flow errors:

- Fixed a bug where declarations from libraries which are exported from one module are not checked properly in the module into which that module is imported (e.g. if `A.foo()` returns a `Promise`, and module B requires A and calls `A.foo()`, the return type of `A.foo()` was not being checked properly)
- Fixed enforcement of Object and Function type annotation arity, so that `Object<K, V>` errors
- Restricted valid computed properties, such that only strings and numbers are allowed (notably, disallows `null` and `undefined`)

New features:

- Added support for `for-of` and support for `Iterable` interface(s)
- Added support for `async`/`await`
- Added structural subtyping for interfaces -- anything can be an instance of an interface as long as it looks right
- Added support for type annotations of the form `typeof x`, where `x` is the name of an in-scope variable
- Added a new config option `suppress_comment`, a regexp which matches against comments and causes any Flow error on the next line to be suppressed. For example, `suppress_comment=.*\$FlowFixMe.*` will cause `/* $FlowFixMe */\nvar x : number = "oops";` to not raise an error.
- Added a new config option `module.name_mapper`, a regexp -> replacement template tuple to be applied to any matching module names before the Flow system resolves the name and looks it up
- Added a `--color=always|never|auto` CLI option, useful when piping to `less -R`
- Added a `--one-line` CLI option which replaces `\n` with `\\n` in multiline error messages, useful when piping to `grep`

Misc:

- Many improvements to library files, especially node and ES6 APIs
- Improved warnings on unsupported class members [PR #461]
- Added support for `export default class`
- Fixed `if (x instanceof Array)`
- Fixed the type of `x && y` when `x` is an array, object or function
- Fixed the `flow get-def` command, especially around imported types
- Fixed a bug with `==` and improved comparison-related error messages
- Fixed file watching for individual files included via .flowconfig [includes]
- Fixed the build ID, so that the server restarts when accessed from a mismatched client version
- Added a new config option `log.file` which overrides the default log file path


###v0.11.0

- We are now syncing Flow's [commit history](https://github.com/facebook/flow/commits/master) to GitHub. No more huge updating diffs. We'll also filter the changelog to the most important things.
- Big React refactoring to support ES6 React classes
- Do you use `any` to workaround things that you want to fix later? Well, now you can use `$FixMe` instead of `any` and easily grep for these workarounds later.
- We now report parsing errors in non-@flow files that you are `require()`'ing/`import`'ing
- Better error messages and better error message positions
- Better error traces, with `flow check --traces N`, where `N` is the trace depth
- Basic support for `Object.freeze()`
- Assorted fixes and updates to the flow libs
- We're trying to be better about commenting the code

###v0.10.0

- Bump version to 0.10.0
- Support import/export type
- [PR #412] More fs implementation
- Support for static overloads
- Fix order of args matched with overloads
- Allow methods to have properties
- [PR #405] Make interface-defined modules work with CommonJS/ES6Module interop
- Refine mixed
- Fix typeof null === "object" refinement
- [PR #397] Facilitate debug builds
- [PR #401] Improve error message clarity in a few places
- Remove 'everything is a bool' implicit cast
- Fix issues with optional properties during InstanceT ~> ObjT
- Fix abnormals in catch blocks

###v0.9.2

- Fix lowercasing issue where "flow Path/To/Root" became "flow path/to/root"
- Fix "My Path/To Flow/flow path/to/root" not autostarting server due to spaces

###v0.9.1

- Unbreak the command line for "flow path/to/root" (thanks samwgoldman for the report!)

###v0.9.0

- Bump version to 0.9.0
- Add Video-, Audio and TextTrackList (+ dependencies)
- Refine switch cases
- Better Not_found error
- [PR #333] Partial fs module implementation
- Update parser to match esprima-fb
- [PR #370] Dom canvas context
- [PR #393] Symbol should be called statically. Fix #391
- Fix havoc_env to copy the `for_type` value
- add bash completion
- rewrite all the commands to use CommandSpec
- json_of_t
- [PR #384] Env printer
- [PR #383] Add some build/test artifacts to .gitignore
- [PR #377] Add missing HTMLImageElement properties
- [PR #367] Re-add merged out oneOfType proptype implementation
- Allow users to pin their .flowconfig to a specific version
- colorize test failure diffs
- Object spread fix
- [PR #322] Add type signature for node "url", "querystring" and "path" modules
- command parsing library
- look for package.json in include paths
- clear module errors properly
- add verbose mode

###v0.8.0

- Bump version to 0.8.0
- [PR #356] Add React.version to lib/react.js
- [PR #331] node http.get() implementation
- Fixes for indexers in object types and objects used as dictionaries
- Flow server speedup
- Support both commas and semicolons in object type patterns
- Fix object assign for unresolved objects
- Add prerr_endlinef
- Fix support for the mixin pattern of specifying super
- Incremental typechecking fix
- Fix up promises interface definition + more tests
- Perf win by loading master_cx once per job
- Support for-in of null and undefined
- Support typeof x === 'boolean'
- Refine true and false literals
- Fix refinement unit test
- Type declarations for Geolocation interface
- [PR #321] Support for React.cloneElement in 0.13
- Refine simple assignments (things like while (x = x.parent) { x.doStuff(); })

###v0.7.0

- Bump version to 0.7.0
- Initial support for ES6 import/export (with CommonJS interop)
- Updates to CSS/CSSOM interface definitions
- propTypes in React components now dictate the concrete type signature for this.props
- Show errors in type-at-pos output
- Flow now watches include paths specified in .flowconfig for file changes
- Interpret `x != undefined` and `x != null` as the same refinement
- Use Object.prototype.hasOwnProperty() calls as a refinement
- Updates to Element and HTMLElement interface definitions

###v0.6.0

- Bump version to 0.6.0
- Also watch for changes in include paths
- Fix the Function type to be any function
- Add Symbol global name to libs
- Support trailing commas in parser
- Make methods immutable
- Remove tuple array length limit
- [PR #315] Implement more PropTypes
- Update componentWillReceive spec
- Unsuppress library errors

###v0.5.0

- Bump version to 0.5.0
- Add HTMLAnchorElement
- [PR #295] Add one of proptype
- Update the parser to work with the new esprima-fb & ast-types
- [PR #299] Declare prompt function (fixes #204)
- [PR #303] Add String.prototype.endsWith()
- quick fix for react/es6 notation
- extend scope of type params to following bounds
- reasonless shortcut
- Add React.findDOMNode to lib/react.js
- move command documentation into each command
- fix path in --help usage example
- fix predicate filtering of null and undefined
- [PR #292] Types for React synthetic events
- basic support for bounded polymorphism
- infer falsiness
- node haste module support
- add Abnormal.string

###v0.4.0

** Flow comments should be ready for use **

- Bump version to 0.4.0
- [PR #259] Add declaration for Document.execCommand()
- [PR #265] flow status --json should exit 2 on errors
- Fix issue with switch statement breaks
- abnormal exit effects of fall-through cases in switch
- Let AnyObjT flow to KeyT
- invariant(false, ...) always throws
- Improve the "Object" annotations
- sealed object literals
- fix return types which could be undefined
- Support type annotations in comments
- [PR #263] disable colors when TERM=dumb
- Add lint checks for TRUE / FALSE / NULL
- class subtyping
- array literal inference
- Add unsafe_xhp typechecker option
- work around false positive conflict markers
- Better dumping types for debugging

###v0.3.0

** Type casts and import type should be ready for use **

- Bump version to 0.3.0
- Move haste module name preprocessing into module_js
- [PR #260] Add SyntheticEvent to lib/react.js
- Add import type to Flow
- [Parser] Add esprima tests for typecasts
- Add support for a module name preprocessor that can re-write require()d module names
- [Parser] Support import types
- Allow absolute paths for [libs] and [include]
- prepare flow-parser to be published to npm
- [PR #247] Resubmit #113 this fixes #155
- [PR #246] Fixes #195

###v0.2.0

- Bump version to 0.2.0
- Fix refinement of optional nullable types
- Typecast expressions (warning - not added to strip out transform yet)
- treat throw as similar to return
- Switch from React to react (React still included until https://github.com/facebook/react/pull/3031)
- Fix types for sessionStorage and Storage
- tighter type for undefined values
- no lib paths fix
- Fix exporting types for builtin modules
- Add Number.isNaN and Number.isSafeInteger type annotations
- Optional properties in objects & optional type revamp

###v0.1.6

- Bump version to 0.1.6
- declare modules that redefine exports instead of exporting multiple things
- Object spread race condition fix
- Fix fallback path to flowlib.tar.gz
- [Parser] Type grouping bug fix
- Add Object.is
- uncomment `delete` method signatures
- Add MAX_SAFE_INTEGER and MIN_SAFE_INTEGER to Number
- make param annotations strict upper bounds

###v0.1.5

- Bump version to 0.1.5
- [PR #223] from commonlisp/master
- [PR #221] from teppeis/Javascript-to-JavaScript
- Better types for React.PropTypes
- Make React a module
- Check PropTypes as a normal object too
- Better support for object call properties
- Add range/offset information to Ast.Loc.t
- ignored react attributes key & ref passed in spread properties
- ignored react attributes key & ref
- Add some checks when upper bound object type has indexer
- fix type arg arity exception
- add traces to .flowconfig
- fix error message when undeclared prop is set by jsx
- [Parser] Actually test ArrayTypeAnnotation
- Fix problem with returns in catch
- JSX with spread attributes
- Incremental mode fix
- Add missing constructors to TypedArray declarations
- [Parser] Update Flow's test suite to match updated esprima

###v0.1.4

- Bump Flow version to 0.1.4
- [Flow] Unbreak the open source build
- [PR #200] from fmahnke/node_http_interface
- [PR #203] from rsolomo/node-net-isip
- [PR #201] from unknownexception/patch-1
- [Flow] Fill out Map definition
- try-catch
- missing annotation errors for type aliases
- Type refinement fixes for early return

###v0.1.3

- Handle strict undefined checks.
- Add theoretical support to print tuple types properly.
- Relax the callback type in Array.prototype.forEach
- Add basic autocomplete support for React classes.
- [Hack] Kill shell.ml entirely.
- Handle spread attributes in JSX syntax.
- [Parser] Allow most keywords as types
- Add Array.from to lib file
- [#161]: String literal keys for object type aliases
- Generalize mock module handling under Haste a bit.
- Improve autocomplete type printing
- misc cleanup
- [Parser] Improve type keywords
- [Parser] Stop vendoring in ast-types
- Add an ocaml wrapper for find path1 ... pathN -name pattern
- Restart if a known lib file changes
- Add ProgressEvent definition and update two onprogress types
- [Hack] Add client time to server start time.
- Improve control flow type refinements
- [Hack] Reduce dependencies on ParserHeap.
- Exit server on config change for now rather than restart to avoid some bugs
- [Parser] ArrowFunction object & array patterns must be wrapped with parens
- Properly functorize logging code too
- [Hack] Make GC parameters configurable.
- Refinements on method calls
- [Hack] Log whether we are loading from a saved state in INIT_END.
- [Hack] Improve error handling for saved states.
- [PR #137] from sverrejoh/master
- [PR #150] from unknownexception/fix/dom.js
- [PR #146] from k-bx/master
- [PR #151] from unknownexception/feature/no_flowlib
- [PR #144] from rsolomo/node-assert
- [PR #191] from Shraymonks/HTMLBaseElement
- [PR #145] from rsolomo/node-util
- [PR #192] from Shraymonks/HTMLScriptElement
- Add ES6 String.prototype.contains
- Add Promise.prototype.done() to Flow lib

###v0.1.2

- [#78] Fallback to index.js if no main attribute in package.json
- [#82] Added ES6 functions to the Math object
- [#94] Start a node stdlib interface file
- [#98] Declare optional parameters according to HTML standard
- [#108] Add buffer class to node.js interface
- [#115] Some more node std libs
- The module system can now be specified in the .flowconfig file
- Libraries can now be specified in the .flowconfig file
- Multiple library paths can be specified
- Added a command to typecheck a file passed on standard input (for IDEs)
- Added HTMLInputElement to the standard library
- Improvements to type printing in flow suggest and other commands
- Fixes to various issues where parser errors were being suppressed

###v0.1.1

- [Issue #4] Typecheck .jsx files
- [Issue #22] Return a nonzero exit code from 'flow check' if there are errors
- [IRC report] Autostart the Flow server on all commands
- Improve the printing of types in 'flow suggest'
- Add a --timeout option to all commands
- [PR #27] Clearer error message when 'flow init' hasn't been run
- [PR #39] Fix to Emacs bindings
- [PR #53] Support node modules that end in .js
- [PR #59] Fix example
- [PR #65] Fix dependencies in flux-chat example
- [PR #66] Add type definitions for HTMLCanvasElement

###v0.1.0

Initial release
