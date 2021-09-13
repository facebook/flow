---
layout: guide
---

## Upgrade tooling <a class="toc" id="toc-upgrade-tooling" href="#toc-upgrade-tooling"></a>
To enable Flow Enums in your repo, you must first update the following packages:

- Upgrade to at least Flow 0.159
  - Flow needs to have some configuration set to enable enums - see below.
- Upgrade [Prettier](https://prettier.io/) to at least version 2.2
  - As of that version, Prettier can handle parsing and pretty printing Flow Enums out of the box.
  - You must use the `flow` [parser option](https://prettier.io/docs/en/options.html#parser) for JavaScript files to be able to format Flow Enums.
- Upgrade [Babel](https://babeljs.io/) to at least version 7.13.0
  - As of that version, Babel can parse Flow Enums. However, to enable this parsing some configuration needs to be supplied,
    and additionally it does not include the transform required - see below.
- Upgrade [jscodeshift](https://github.com/facebook/jscodeshift) to at least version 0.11.0
- Upgrade [hermes-parser](https://www.npmjs.com/package/hermes-parser) to at least version 0.4.8
- For ESLint, either:
  - Use [hermes-eslint](https://www.npmjs.com/package/hermes-eslint) as your ESLint parser, at least version 0.4.8
  - Or upgrade [babel-eslint](https://github.com/babel/babel-eslint) to version 10.1.0
    - As of that version, `babel-eslint` can handle Flow Enums out of the box.
    - Do not upgrade to 11.x, this branch does not support Flow Enums.
  - Or use another solution using Babel 7.13.0 or later, with Flow enabled - this may also work

If you have any other tool which examines your code, you need to update it as well. If it uses [flow-parser](https://www.npmjs.com/package/flow-parser),
[hermes-parser](https://www.npmjs.com/package/hermes-parser) or `@babel/parser`, upgrade those as per the instructions above.
If it uses some other parser, you will need to implement parsing Flow Enums in that parser. You can look at the existing code in Babel, Flow, and Hermes parsers to guide your work.


## Enable enums <a class="toc" id="toc-enable-enums" href="#toc-enable-enums"></a>
- In your `.flowconfig`, under the `[options]` heading, add `enums=true`
- Add the Flow Enums Babel transform. It turns enum declaration AST nodes into calls to the runtime:
  [babel-plugin-transform-flow-enums](https://www.npmjs.com/package/babel-plugin-transform-flow-enums).
  Add it to your development dependencies and adjust your Babel configuration to use the transform.
  The transform by default requires the runtime package directly (below), but you can configure this.
- Add the Flow Enum runtime package to your production dependencies.
  This will be required and used at runtime to create Flow Enums: [flow-enums-runtime](https://www.npmjs.com/package/flow-enums-runtime)


## Enable suggested ESLint rules <a class="toc" id="toc-enable-suggested-eslint-rules" href="#toc-enable-suggested-eslint-rules"></a>
Enums can be exhaustively checked in `switch` statements, so may increase the use of `switch` statements compared to before.
To prevent common issues with `switch` statements, we suggest you enable these ESLint rules (at least as warnings):

- [no-fallthrough](https://eslint.org/docs/rules/no-fallthrough):
  This prevents the user from accidentally forgetting a `break` statement at the end of their switch case, while supporting common use-cases.
- [no-case-declarations](https://eslint.org/docs/rules/no-case-declarations):
  This prevents lexicaly scoped declarations (`let`, `const`) from being introduced in a switch case, without wrapping that case in a new block.
  Otherwise, declarations in different cases could conflict.

We also have some Flow Enums specific rules as part of [eslint-plugin-fb-flow](https://www.npmjs.com/package/eslint-plugin-fb-flow):
- [use-flow-enums](https://www.npmjs.com/package/eslint-plugin-fb-flow#use-flow-enums):
  Suggests that enum-like `Object.freeze` and `keyMirror` usage be turned into Flow Enums instead.
- [flow-enums-default-if-possible](https://www.npmjs.com/package/eslint-plugin-fb-flow#flow-enums-default-if-possible):
  Auto-fixes string enums with specified values identical to the member names to defaulted enums.
- [no-flow-enums-object-mapping](https://www.npmjs.com/package/eslint-plugin-fb-flow#no-flow-enums-object-mapping):
  Suggests using a function with a switch to map enum values to other values, instead of an object literal.
