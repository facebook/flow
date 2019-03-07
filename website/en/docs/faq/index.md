---
layout: guide
---

### I checked that `foo.bar` is not `null`, but Flow still thinks it is. Why does this happen and how can I fix it? <a class="toc" id="toc-i-checked-that-foo-bar-is-not-null-but-flow-still-thinks-it-is-why-does-this-happen-and-how-can-i-fix-it" href="#toc-i-checked-that-foo-bar-is-not-null-but-flow-still-thinks-it-is-why-does-this-happen-and-how-can-i-fix-it"></a>

Flow does not keep track of side effects, so any function call may potentially nullify your check.
This is called [refinement invalidation](https://flow.org/en/docs/lang/refinements/#toc-refinement-invalidations).


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

You can get around this by storing your checked values in local variables:

```js
// @flow
type Param = {
  bar: ?string,
}
function myFunc(foo: Param): string {
  if (foo.bar) {
    const bar = foo.bar;
    console.log("checked!");
    return bar; // Ok!
  }

  return "default string";
}
```

### I checked that my object is of type A, so why does Flow still believe it's A | B? <a class="toc" id="toc-i-checked-that-my-object-is-of-type-a-so-why-does-flow-still-believe-it-s-a-b" href="#toc-i-checked-that-my-object-is-of-type-a-so-why-does-flow-still-believe-it-s-a-b"></a>

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

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQAXAngBwKagEqYM7qwB2+2AvKIqKAD6gDeoaWAXKAOT4CuAxrwXwcANKABuAQ2jdM7fMgBOAS2IBzUAF9qdRswyzOmBQtgKRoIyYXsAosdOaA3IkS8S80AAsJxACbRMPEJ3ClAACgUCIlIDIOiyAEpQcgA+Rm0lSHDI4JiAOhYKckouPgF8ISSGbRo3UmRxKRlk0Bz4zDzJaUwa0DJkABUlAFtMWG5kMLCk1PSaedA6-FgAvLhVMIADABIGLpkNUABGTYTejVEjgAYbhOcaLQ1HIA)):
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

### I'm in a closure and Flow ignores the if check that asserts that `foo.bar` is defined. Why? <a class="toc" id="toc-i-m-in-a-closure-and-flow-ignores-the-if-check-that-asserts-that-foo-bar-is-defined-why" href="#toc-i-m-in-a-closure-and-flow-ignores-the-if-check-that-asserts-that-foo-bar-is-defined-why"></a>

In the previous section we showed how refinement is lost after a function call. The exact same thing happens within closures, since
Flow does not track how your value might change before the closure is called.

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

### But Flow should understand that this function cannot invalidate this refinement, right? <a class="toc" id="toc-but-flow-should-understand-that-this-function-cannot-invalidate-this-refinement-right" href="#toc-but-flow-should-understand-that-this-function-cannot-invalidate-this-refinement-right"></a>

Flow is not [complete](../lang/types-and-expressions/#soundness-and-completeness), so it cannot check all code perfectly. Instead,
Flow will make conservative assumptions to try to be sound.

### Why can't I use a function in my if-clause to check the type of a property? <a class="toc" id="toc-why-can-t-i-use-a-function-in-my-if-clause-to-check-the-type-of-a-property" href="#toc-why-can-t-i-use-a-function-in-my-if-clause-to-check-the-type-of-a-property"></a>
Flow doesn't track refinements made in separated function calls.

Example ([https://flow.org/try](https://flow.org/try/#0MYewdgzgLgBAhgEwTAvDAFAMwJYCdoBcMYArgLYBGAprgDQwRWhgJGmU0CUqAfDDvlgBqBk3AIA3AChm0GADc4AGyLRc2MAHMYAH2LlquVDACM02bGwQAcgZrH0ipSSoAVEACUqOMFTZ2jPTUNTW4UPigATwAHKhBMBWUXdy8fKlQUNABydkMs6WwE9CtbDlxHZU5uRAQKpXoAJk4JIA))
```js
// @flow
const add = (first: number, second: number) => first + second;
const val: string | number = ...
const isNumber = (valueToRefine: ?number) => typeof valueToRefine === 'number';
if (isNumber(val)) add(val, 2);
```

However, Flow has [predicates functions](https://flow.org/en/docs/types/functions/#toc-predicate-functions) that can do these checks via `%checks`.

Example ([https://flow.org/try](https://flow.org/try/#0MYewdgzgLgBAhgEwTAvDAFAMwJYCdoBcMYArgLYBGAprgDQwRWhgJGmU0CUqAfDDvlgBqBk3AIA3AChm0GADc4AGyLRc2MAHMYAH2LlquVDACM02bGwQAcgZrH0ipSSoAVEACUqOMFTZ2jPTUNTU4iAFJgAAsmAGsIXhgoAE8AByoQTAVlF3cvHypUFDQAcnZDEulsLPQrWw5cR2VObkQEJqV6ACZOCSA))
```js
// @flow
const add = (first: number, second: number) => first + second;
const val: string | number = ...
const isNumber = (valueToRefine: ?number): %checks => typeof valueToRefine === 'number';
if (isNumber(val)) add(val, 2);
```

### I got a "Missing type annotation" error. Where does it come from? <a class="toc" id="toc-i-got-a-missing-type-annotation-error-where-does-it-come-from" href="#toc-i-got-a-missing-type-annotation-error-where-does-it-come-from"></a>

Flow requires type annotations at module boundaries to make sure it can scale. To read more about that, check out our [blog post](https://medium.com/flow-type/asking-for-required-annotations-64d4f9c1edf8) about that.

The most common case you'll encounter is when exporting a function or React component. Flow requires you to annotate inputs. For instance, in this [example](https://flow.org/try/#0PTAEAEDMBsHsHcBQBTAHgB1gJwC6gMawB2AzngIYAmloAvKOXQHwOgDUoAjANxA), flow will complain:
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

To see how you can annotate exported React components, check out our docs on [HOCs](../react/hoc/#toc-exporting-wrapped-components).

There are other cases where this happens, and they might be harder to understand. You'll get an error like `Missing type annotation for U` For instance, you wrote this [code](https://flow.org/try/#0PTAEAEDMBsHsHcBQiDGsB2BnALqAhgE4F4CeoAvKANoDkeNANKDQEY0C6iApgB4AOsArjRZcAcy7ouBAJYoAgkVIV8SkgDoAtnj4AKPBQB8+AJRA):
```js
// @flow
const array = ['a', 'b']
export const genericArray = array.map(a => a)
```

Here, Flow will complain on the `export`, asking for a type annotation. Flow wants you to annotate exports returned by a generic function. The type of `Array.prototype.map` is `map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>`. The `<U>` corresponds to what is called a [generic](https://flow.org/en/docs/types/generics/), to express the fact that the type of the function passed to map is linked to the type of the array.

Understanding the logic behind generics might be useful, but what you really need to know to make your typings valid is that you need to help Flow to understand the type of `genericArray`.

You can do that by adding an explicit type argument:

```js
// @flow
const array = ['a', 'b'];
export const genericArray = array.map<string>(a => a);
```


or by annotating the exported constant ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBjWA7AzgF1AQwE764CeoAvKANoDku1ANKNQEbUC6iApgB4AOs+HKkw4A5pzSd8AS2QBBQiQBcoBUWIAebDLSiAfOTyLiAOgC2uXgApc5A7gCUQA)):
```js
// @flow
const array = ['a', 'b']
export const genericArray: Array<string> = array.map(a => a)
```

### Flow cannot understand the types of my higher order React component, how can I help it? <a class="toc" id="toc-flow-cannot-understand-the-types-of-my-higher-order-react-component-how-can-i-help-it" href="#toc-flow-cannot-understand-the-types-of-my-higher-order-react-component-how-can-i-help-it"></a>
Typings HOCs can be complicated. While you can follow the [docs about it](https://flow.org/en/docs/react/hoc/), sometimes it can be easier to type the returned component.

For instance, in this [example](https://flow.org/try/#0PTAEAEDMBsHsHcBQBLAtgB1gJwC6gFSgCGAzqAEoCmRAxnpFrKqAORbV0sDciiNsAOxJ4SlHABUAnukqgAvKABCpSgGEmmAZQF45APlDpG6MvtAAeZaPUZB2vAG8AdC6OwTAX1A5plOQCIAIwBXHBxBf1BgPR5+ITwAcW1KLGQaRVDwgXlQAAoHHxkAGlAaAAtkaAATdgEPAEp5A3MQsMFvXzkC3y9BVWg0gGsu3MazOJJYaEonOABzXJYaAZpByiqWeo89B3LKmu0Pc2BWrJjeCbwMtoEALgoOHCcbTXspGXNdiura+6paJ4AOVgVUo2xyogkvlySS0qXSmUE9S4QA), we don't type the HOC (setType), but the component created with it, `Button`. To do so, we use the type `React.ComponentType`.
```js
// @flow
import * as React from 'react';

const setType = BaseComponent => props => <BaseComponent {...props} type="button" />;
const GenericButton = ({type, children}) => <button type={type} onClick={() => console.log('clicked')}>{children}</button>;

const Button: React.ComponentType<{children: React.Node}> = setType(GenericButton);
```
