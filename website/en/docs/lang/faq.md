---
layout: guide
---

Often, Flow will raise errors that you would consider wrong. Most of these issues are actually explained in the presentation of the [type system](https://flow.org/en/docs/lang/) and more specifically in the [soundness and completeness section](https://flow.org/en/docs/lang/types-and-expressions/#toc-soundness-and-completeness).
Several of them are the consequence of careful tradeoffs made by the team, where you have to balance false positives and false negatives (see the above mentioned section).

### I checked that `foo.bar` is not `null`, but Flow still thinks it is. Why does this happen? <a class="toc" id="toc-refinement-invalidation" href="#toc-refinement-invalidation"></a>

You have to keep in mind in JavaScript, you can modify data structures as you want, and Flow takes that in consideration. As of today, Flow cannot identify side-effects, so you end up with Flow considering that a `console.log()` can modify the content of an array (since nothing prevents you from modifying `console.log`). Therefore, whenever you refine types, some of your assumptions might not be valid anymore (this is especially common within closures). This is called [refinement invalidation](https://flow.org/en/docs/lang/refinements/#toc-refinement-invalidations).


Example ([https://flow.org/try](https://flow.org/try/#0C4TwDgpgBACghgJzgWygXigbwFBSgI0QC4oB+AZ2AQEsA7AcwBpsBfbAMwFdaBjYagPa0oyEADFuPABTsBAkvCTIAlCUo0GWXFGrsoMuQDpCCZVrx4eQ8gIA2EQ7YH0pAIh4ALCDwDWEACYAhK7KANzaAJAIEMCcCMKyAsaIoVAA9GlQ7E4A7lAQCAgCCOSGUACSeiACnFBWyMgQtMBQwF511nYOTkw6LTnFPuTabHja0bHxUK7+EOxwnLYt6nT0ruEsQA)):
```js
// @flow
type Param = {
  bar: ?string,
}
function myFunc(foo: Param): string {
  if (foo.bar) {
    console.log("checked!");
  	return foo.bar; // Flow errors. If you remove the console.log, it works
  }

  return "default string";
}
```

### I checked that my object is of type A, but flow still believes it's A | B
Refinement invalidation can also happen with [disjoint unions](https://flow.org/en/docs/types/unions/#toc-disjoint-unions). Any function call will invalidate any refinement.

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQAXAngBwKagEqYM7qwB2+2AvKIqKAD6gDeoaWAXKAOT4CuAxrwXwcANKABuAQ2jdM7fMgBOAS2IBzUAF9qdRswyzOmBQtgKRoIyYXsAosdOaA3IkS8S80AAsJxACbRMPEJ3ClAACgUCIlIDIOiyAEpQcgA+Rm0lSHDI4JiAOhYKckouPgF8ISSGbRoyZAAVJQBbTFhuZDCwpNT0mj7QN1JYALy4VTCAAwASBhz4zDzJaUwNUABGCYSazVE1gAYDhOcaLQ1HIA)):
```js
// @flow
type Response =
  | { type: 'success', value: string }
  | { type: 'error', error: Error };

const handleResponse = (response: Response) => {
  if (response.type === 'success') {
    setTimeout(() => {
      console.log(`${response.value} 1`)
    }, 1000);
  }
};
```

Here, a work around would be to extract the part of the value you're interested in, or to move the if check inside the `setTimeout` call:

Example ((https://flow.org/try)[https://flow.org/try/#0PTAEAEDMBsHsHcBQAXAngBwKagEqYM7qwB2+2AvKIqKAD6gDeoaWAXKAOT4CuAxrwXwcANKABuAQ2jdM7fMgBOAS2IBzUAF9qdRswyzOmBQtgKRoIyYXsAosdOaA3IkS8S80AAsJxACbRMPEJ3ClAACgUCIlIDIOiyAEpQcgA+Rm0lSHDI4JiAOhYKckouPgF8ISSGbRo3UmRxKRlk0Bz4zDzJaUwa0DJkABUlAFtMWG5kMLCk1PSaedA6-FgAvLhVMIADABIGLpkNUABGTYTejVEjgAYbhOcaLQ1HIA]):
```js
// @flow
type Response =
  | { type: 'success', value: string }
  | { type: 'error', error: Error };

const handleResponse = (response: Response) => {
  if (response.type === 'success') {
    const value = response.value
    setTimeout(() => {
      console.log(`${value} 1`)
    }, 1000);
  }
};
```

### I'm in a closure and Flow ignores the if check that asserts that `foo.bar` is defined <a class="toc" id="toc-closure" href="#toc-closure"></a>

In the previous section we showed how refinement is lost after a function call. Sadly, the exact same thing happens within closures.

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQAXAngBwKagAqYE4DOsAdqALygDeAhgOaYBcoA-CQK4C2ARgQL6IAxqULJQWWOmjZKAbVoNmARgBMfADTV6TUEoAcGrYtAqALHwC6AbiEixsaABM8RUsxfEylBToDsABj4bAEtIUAAKB2cCTwA6bQBKakRQcUxJaVjIWHwAURpBAAtwrFcvAD5k1NThEmJMuDpwgAMAFULsUs9QYMJQABIqLtJ4hj5QGhJHUGQO0Cj5kmxegaoojxHtPmaEm1S+BMQ+IA)):
```js
// @flow
type Person = {age: ?number}
const people = [{age: 12}, {age: 18}, {age: 24}];
const oldPerson: Person = {age: 70};
if (oldPerson.age) {
  people.forEach(person => {
    console.log(`The person is ${person.age} and the old one is ${oldPerson.age}`);
  })
}
```

The solution here is to move the if check in the `forEach`, or to assign the `age` to an intermediate variable.

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQAXAngBwKagAqYE4DOsAdqALygDeAhgOaYBcoA-CQK4C2ARgQL6IAxqULJQWWOmjZKAbVoNmARgBMfADTV6TUEoAcGrYtAqALHwC6AbiEixsaABM8RUsxfEylBToDsABj4bAEtIUAAKB2cCTwA6bQBKakRQUGESUVBtClAoj1J4hhtUiSlMWMhYfABRGkEAC3CsVy8APmTU1PTiaVi4OnCAAwAVeuxmz1BgwlAAEioJgu0+LJJHUGQx3KdckmxpuZ8+QYTi0D4ExD4gA)):
```js
// @flow
type Person = {age: ?number}
const people = [{age: 12}, {age: 18}, {age: 24}];
const oldPerson: Person = {age: 70};
if (oldPerson.age) {
  const age = oldPerson.age;
  people.forEach(person => {
    console.log(`The person is ${person.age} and the old one is ${age}`);
  })
}
```


### Why does Flow believe that Foo is not a subtype of Bar? <a class="toc" id="toc-invalid-subtyping" href="#toc-invalid-subtyping"></a>

Another difficulty people encounter is when they want to use a type different than the one declared that they believe compatible (as a parameter to a function for instance), and Flow complains, telling that the two types are not compatible. This is caused by invalid sub-typing.

Example ([https://flow.org/try](https://flow.org/try/#0C4TwDgpgBA6glsAFgUQE6oPaqgXigbwCgoSpUIBnAFXAgC4oLhU4A7AcwBpjSJ0sGaTKm4BfANyFQkWAkQBZAIYgARhCFZcBHiXLVaDJiw7dSUPsIYB+DSMITCAMwCurAMbA4GVlESLWACYANhAAgqwUAO58ABRgiqiKALYM8EhKqur8qACUBKKEbt5MZJSpcrZa+Ho0kAwA5JEV2fWc5tkMrBCRULYxOQ5+gSHhUbF6OeJQAPTTUABiQRg9TUFBUEVJYEGKbL58EEA)):
```js
// @flow
type WithError = {
    resType: string,
    error: Error,
};
type WithMaybeError = {
    resType: string,
    error: ?Error,
};
function handleAnswer(param: WithMaybeError) {}
const res: WithError = {resType: 'withError', error: new Error()};
handleAnswer(res); // Flow will complain here
```

The previous snippet of code seems correct, indeed `handleAnswer` requires a `resType`, provided by `res`, and doesn't need the error property. However, you need to consider the case where `handleAnswer` mutates its parameter.

```js
// @flow
function handleAnswer(param: WithMaybeError) {
    param.error = null;
}
const res: WithError = {resType: 'withError', error: {code: 404}};
handleAnswer(res);
console.log(res.error);
```
The last statement doesn't work anymore, since `res.error` has been set to null, however, `res` is a `WithError`, so it should always have an `error` field. This is the reason why Flow prevents you from doing so. To circumvent this problem, you have to indicate your [properties as covariant](https://flow.org/blog/2016/10/04/Property-Variance/) (or read-only).
```js
// @flow
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

### But Flow should understand that this function cannot invalidate this refinement, right? <a class="toc" id="toc-pure-functions" href="#toc-pure-functions"></a>

Flow, not being a perfect type system fails to recognize some patterns. Firstly, Flow doesn't make the distinction between a pure and an unpure function. We previously saw that Flow considers invalidates any refinement after a function call, it should sometimes be able to understand that some functions are pure, and that the invalidation is not necessary.

Example ([https://flow.org/try](https://flow.org/try/#0MYewdgzgLgBAZiEAuGBvARgQwE4oPzTYCWYA5gL4wC8aWuMA5AJKxEQyYyEmkPkDcAKFCRYmACbjqMABRwi2aABouAUxHiAlNQB88BdBgBqNRqFE4shCAB0dbakExnMEYYgBXALbSJ4mQCMKgBMmkIuruCG6NIABgAkqNZ2OJSJAMpQxGQynl6a5LFC5EA)):
```js
// @flow
const foo: {bar: ?string} = {bar: 'It is a string'};
const add = (first, second) => first + second;
if (foo.bar) {
    const sum = add(1, 2);
    const b = `${foo.bar} ${String(sum)}`;
}
```

### Why can't I use a function in my if-clause to check the type of a property? <a class="toc" id="toc-predicates" href="toc-predicates"></a>
Moreover, Flow doesn't understand refinements made in separated function calls. This mean that by default, Flow will not try to use the content of a function for any refinement.

Example ([https://flow.org/try](https://flow.org/try/#0MYewdgzgLgBAhgEwTAvDAFAMwJYCdoBcMYArgLYBGAprgDQwRWhgJGmU0CUqAfDDvlgBqBk3AIA3AChm0GADc4AGyLRc2MAHMYAH2LlquVDACM02bGwQAcgZrH0ipSSoAVEACUqOMFTZ2jPTUNTW4UPigATwAHKhBMBWUXdy8fKlQUNABydkMs6WwE9CtbDlxHZU5uRAQKpXoAJk4JIA))
```js
// @flow
const add = (first: number, second: number) => first + second;
const val: string | number = ...
const isNumber = (valueToRefine: ?number) => typeof valueToRefine === 'number';
if (isNumber(val)) add(val, 2);
```

This case being a common one, Flow has [predicates functions](https://flow.org/en/docs/types/functions/#toc-predicate-functions) that can do these checks, thanks to the keyword `%check`.

Example ([https://flow.org/try](https://flow.org/try/#0MYewdgzgLgBAhgEwTAvDAFAMwJYCdoBcMYArgLYBGAprgDQwRWhgJGmU0CUqAfDDvlgBqBk3AIA3AChm0GADc4AGyLRc2MAHMYAH2LlquVDACM02bGwQAcgZrH0ipSSoAVEACUqOMFTZ2jPTUNTU4iAFJgAAsmAGsIXhgoAE8AByoQTAVlF3cvHypUFDQAcnZDEulsLPQrWw5cR2VObkQEJqV6ACZOCSA))
```js
// @flow
const add = (first: number, second: number) => first + second;
const val: string | number = ...
const isNumber = (valueToRefine: ?number): %checks => typeof valueToRefine === 'number';
if (isNumber(val)) add(val, 2);
```

### I got a "Missing type annotation". Where does it come from?
Flow has some requirements on what you need to annotate both for to have more precise typings and for better performance. If you're interested to know more about that, the team wrote a [blog post](https://medium.com/flow-type/asking-for-required-annotations-64d4f9c1edf8) about that.

The most common case you'll encounter is when exporting a function. Flow requires you to annotate inputs. For instance, in this [example](https://flow.org/try/#0PTAEAEDMBsHsHcBQBTAHgB1gJwC6gMawB2AzngIYAmloAvKOXQHwOgDUoAjANxA), flow will complain:
```js
// @flow
export const add = a => a + 1;
```

The fix here is to add types to the parameters of `add`.

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBTAHgB1gJwC6gMawB2AzngIYAmloAvKABTkBcoRArgLYBGyWAlHQB8ocqADUoAIwBuIA)):
```js
// @flow
export const add = (a: number) => a + 1;
```

There are other cases where this happens, and they might be harder to understand. You'll get an error like `Missing type annotation for U` For instance, you wrote this [code](https://flow.org/try/#0PTAEAEDMBsHsHcBQiDGsB2BnALqAhgE4F4CeoAvKANoDkeNANKDQEY0C6iApgB4AOsArjRZcAcy7ouBAJYoAgkVIV8SkgDoAtnj4AKPBQB8+AJRA):
```js
// @flow
const array = ['a', 'b']
export const genericArray = array.map(a => a)
```

Here, Flow will complain on the `export`, asking for a type annotation. Flow wants you to annotate exports returned by a generic function. The type of `Array.prototype.map` is `map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>`. The `<U>` corresponds to what is called a [generic](https://flow.org/en/docs/types/generics/), to express the fact that the type of the function passed to map is linked to the type of the array.

Understanding the logic behind generics might be useful, but what you really need to know to make your typings valid is that you need to help Flow to understand the type of `genericArray`.

You can do that by annotating the exported constant ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBjWA7AzgF1AQwE764CeoAvKANoDku1ANKNQEbUC6iApgB4AOs+HKkw4A5pzSd8AS2QBBQiQBcoBUWIAebDLSiAfOTyLiAOgC2uXgApc5A7gCUQA)):
```js
// @flow
const array = ['a', 'b']
export const genericArray: Array<string> = array.map(a => a)
```

### Flow cannot understand the types of my higher order React component, how can I help it?
Typings HOCs can be complicated. While you can follow the docs about [it](https://flow.org/en/docs/react/hoc/), sometimes it can be easier to type the returned component.

For instance, in this [example](https://flow.org/try/#0PTAEAEDMBsHsHcBQBLAtgB1gJwC6gFSgCGAzqAEoCmRAxnpFrKqAORbV0sDciiNsAOxJ4SlHABUAnukqgAvKABCpSgGEmmAZQF45APlDpG6MvtAAeZaPUZB2vAG8AdC6OwTAX1A5plOQCIAIwBXHBxBf1BgPR5+ITwAcW1KLGQaRVDwgXlQAAoHHxkAGlAaAAtkaAATdgEPAEp5A3MQsMFvXzkC3y9BVWg0gGsu3MazOJJYaEonOABzXJYaAZpByiqWeo89B3LKmu0Pc2BWrJjeCbwMtoEALgoOHCcbTXspGXNdiura+6paJ4AOVgVUo2xyogkvlySS0qXSmUE9S4QA), we don't type the HOC (setType), but the component created with it, `Button`. To do so, we use the type `React.ComponentType`.
```js
// @flow
import * as React from 'react';

const setType = BaseComponent => props => <BaseComponent {...props} type="button" />;
const GenericButton = ({type, children}) => <button type={type} onClick={() => console.log('clicked')}>{children}</button>;

const Button: React.ComponentType<{children: React.Node}> = setType(GenericButton);
```
