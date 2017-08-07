---
layout: guide
---

Often, Flow will raise errors that you would consider wrong. Most of these issues are actually explained in the presentation of the [type system](https://flow.org/en/docs/lang/) and more specifically in the [soundness and completeness section](https://flow.org/en/docs/lang/types-and-expressions/#toc-soundness-and-completeness).
Several of them are the consequence of careful tradeoffs made by the team, where you have to balance false positives and false negatives (see the above mentionned section).

### Common user errors <a class="toc" id="toc-common-user-errors" href="#toc-common-user-errors"></a>

#### Refinement invalidation

You have to keep in mind in JavaScript, you can modify data structures as you want, and Flow takes that in consideration. As of today, Flow cannot identify side-effects, so you end up with Flow considering that a `console.log()` can modify the content of an array (since nothing prevents you from modifying `console.log`). Therefore, whenever you refine types, some of your assumptions might not be valid anymore (this is especially common within closures).

#### Invalid subtyping

Another difficulty people encounter is when they want to use a type different than the one declared that they believe compatible (as a parameter to a function for instance), and Flow complains, telling that the two types are not compatible.

```js
type WithError = {
    resType: string,
    error: Object,
};
type WithMaybeError = {
    resType: string,
    error: ?Object,
};
function handleAnswer(param: WithMaybeError) {}
const res: WithError = {resType: 'withError', error: {code: 404}};
handleAnswer(res); // Flow will complain here
```

The previous snippet of code seems correct, indeed handleAnswer require a `resType`, provided by `res`, and doesn't need the error property. However, you need to consider the case where `handleAnswer` mutates its parameter.

```js
function handleAnswer(param: WithMaybeError) {
    param.error = null;
}
const res: WithError = {resType: 'withError', error: {code: 404}};
handleAnswer(res);
console.log(res.error);
```
The last statement doesn't work anymore, since `res.error` has been set to null, however, res is a WithError, so it should always have an error field. This is the reason why Flow prevents you from doing so. To circumvent this problem, you have to indicate your properties as covariant (or read-only).
// TODO link doc section
```js
type WithError = {
    +resType: string,
    +error: Object,
};
type WithMaybeError = {
    +resType: string,
    +error: ?Object,
};
function handleAnswer(param: WithMaybeError) {}
const res: WithError = {resType: 'withError', error: {code: 404}};
handleAnswer(res); // Flow is happy!
```

### Errors due to Flow limitations <a class="toc" id="toc-flow-errors" href="#toc-flow-errors"></a>

Flow, not being a perfect type system fails to recognize some patterns. Firstly, Flow doesn't make the distinction between a pure and an unpure function. We previously saw that Flow considers invalidates any refinement after a function call, it should sometimes be able to understand that some functions are pure, and that the invalidation is not necessary.
```js
const a: ?string = 'It is a string';
const add = (first, second) => first + second;
if (a) {
    const sum = add(1, 2);
    const b = `${a} ${String(sum)}`;
}
```

Moreover, Flow doesn't understand refinements made in separated function calls. It is something in consideration, but as of today, you will have to inline your checks.
```js
const add = (first: number, second: number) => first + second;
const val: string | number = ...
const isNumber = (valueToRefine: ?number) => typeof valueToRefine === 'number';
if (isNumber(val)) add(val, 2);
```

