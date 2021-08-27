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

### Why can't I pass an `Array<string>` to a function that takes an `Array<string | number>` <a class="toc" id="toc-why-can-t-i-pass-an-array-to-a-function-that-takes-an-array" href="#toc-why-can-t-i-pass-an-array-to-a-function-that-takes-an-array"></a>

The function's argument allows `string` values in its array, but in this case Flow prevents the original array from receiving a `number`. Inside the function, you would be able to push a `number` to the argument array, causing the type of the original array to no longer be accurate. You can fix this error by changing the type of the argument to `$ReadOnlyArray<string | number>`. This prevents the function body from pushing anything to the array, allowing it to accept narrower types.

As an example, this would not work:

```js
// @flow

const fn = (arr: Array<string | number>) => {
  // arr.push(123) NOTE! Array<string> passed in and after this it would also include numbers if allowed
  return arr;
};

const arr: Array<string> = ['abc'];

fn(arr); // Error!
```

but with `$ReadOnlyArray` you can achieve what you were looking for:

```js
// @flow
const fn = (arr: $ReadOnlyArray<string | number>) => {
  // arr.push(321) NOTE! Since you are using $ReadOnlyArray<...> you cannot push anything to it
  return arr;
};

const arr: Array<string> = ['abc'];

fn(arr);
```

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQiSgOoEsAuALWBXLUAJwFMBDAE1gDtoBPALmQGNaBnIyGgRlAF5QACnLFijUAEEx5egB5OxDDQDmoAD4ACmvgC2AI1LEAfAEoBx0AG9EASFSjiAOgAO+djiE8ATAGZzqAByAPIAKgCiAIRSMvKKyiqWLuTs7KSUoMqg5DQZ5JBYRqC4GOyZRPAE0HnQ7LCZNCzQ+JSkoDoGRmUYkNnQcPDpdmRY+MQ02WIA3IgAvqwcRI48EtLEsgpYSqqWggDaAOTk+iwHALozKGDcPCJiplOgqHIAtG+g+I2wurqkNFywYigIzEQHsSKRZCoN4w2Fw+EIxFIqFgABKFGodHoLwAbl0MLRmIg2DROKBuN4BMJHBIACToqjBLFrDbxVQabR6Qwmcz8Sw2UCCp5gRyudyeXzeHgBMAhCLRADKyhYbXoBEmbXcCVA9IxTIYLPkTmNljV+FALByNFgRDcHmyNHoJXZWHq2GGpFG40mxDmC1JSzE3lWsU220SVMOx1OF2QFLuxG8DyAA))

### Why can't I pass `{ a: string }` to a function that takes `{ a: string | number }` <a class="toc" id="toc-why-can-t-i-pass-a-string-to-a-function-that-takes-a-string-number" href="#toc-why-can-t-i-pass-a-string-to-a-function-that-takes-a-string-number"></a>

The function argument allows `string` values in its field, but in this case Flow prevents the original object from having a `number` written to it. Within the body of the function you would be able to mutate the object so that the property `a` would receive a `number`, causing the type of the original object to no longer be accurate. You can fix this error by making the property covariant (read-only): `{ +a: string | number }`. This prevents the function body from writing to the property, making it safe to pass more restricted types to the function.

As an example, this would not work:

```js
// @flow

const fn = (obj: {| a: string | number |}) => {
  // obj.a = 123;
  return obj;
};

const object: {| a: string |} = {a: 'str' };

fn(object); // Error!
```

but with a covariant property you can achieve what you were looking for:

```js
// @flow
const fn = (obj: {| +a: string | number |}) => {
  // obj.a = 123 NOTE! Since you are using covariant {| +a: string | number |}, you can't mutate it
  return obj;
};

const object: {| a: string |} = { a: 'str' };

fn(object);
```

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQiSgOoEsAuALWBXLUAJwFMBDAE1gDtoBPALmQGNaBnIyGgRlAF5QAClgAjAFaNQAbwA+oclM7EMNAOahZAApr4AtqNLEtsgL4BKAQD4ZiAJCox4gHTkBoHgCYAzKBAA5AHkAFQBRAEIZbUVQZVUNM1AAB3J2dlJKUFUFGkzySCwjUFwMdiyieAJoTMNQXQMijEgFaDh4DPsyLHxiGlAnAG5EU1YOIidSFiweKTktGLj1TVN3aRiAcmV10FMhlDBuHhEJSenzAb8wAB4AWjvQfBo2PT1SGiJsyFhiUCNib-Y4XCyFQdzB4IhkKh0JhILAACUKNQ6PQbgA3IzsDC0ZiINg0TigbiedzHSRRUAAagWWBUS209UMPzMln4NmkiFAoAcYCcrncXl8ARCEVAAGVVCxSKB6AQFGQHlilmw0eQVOR3lEtNSlLT4podPomcsADQyuUsDXrIh6QjkQrlTqkbq9foSIYjPFjN3iU6eWbyGl0hIrQTSBRSTa07a7ZDEsl+85AA))

### Why can't I refine a union of objects? <a class="toc" id="toc-why-can-t-i-refine-a-union-of-objects" href="#toc-why-can-t-i-refine-a-union-of-objects"></a>

There are two potential reasons:
1. You are using inexact objects.
2. You are destructuring the object. When destructuring, Flow loses track of object properties.

Broken example:

```js
/* @flow */

type Action =
  | {type: 'A', payload: string}
  | {type: 'B', payload: number};

// Not OK
const fn = ({type, payload}: Action) => {
  switch (type) {
    case 'A': return payload.length;
    case 'B': return payload + 10;
  }
}
```

Fixed example:

```js
/* @flow */

type Action =
  | {type: 'A', payload: string}
  | {type: 'B', payload: number};

// OK
const fn = (action: Action) => {
  switch (action.type) {
    case 'A': return action.payload.length;
    case 'B': return action.payload + 10;
  }
}
```

([https://flow.org/try](https://flow.org/try/#0PQKgBAAgZgNg9gdzCYAoVAXAngBwKZgCCAxhgJZwB2YAvGKmGAD5gDe2+AXGAOSE8AaMDgCGWeCIAm3AM4YATmUoBzAL6MGzNhzzceAIUHCxE6WEoBXALYAjPPNUBudMGBgA8gGlUxKnLBQAIy0YAAUIqQUlNwk5FQAlLQAfGyaMghkGMQAFmERcZQAdDqJrJqMxCIyBHw83PJ4GBby1PlRhaLicFKFMHgqGNnOjBVVNYb1jc2tkVQdJt2SYADUYIEADMNgqqg7Lm4AcnAYHp5gFpS+Vlb9J1Bw8mD28g8yAISorr6U-lAATCFQuxcHghJ1TKoYrNKIkaCkyq5GOlMjkwiVUoiRpVqrx+JMmi1jF0en0BkNPm4sWNeBMwA0CdRwYsVmtNhTGDtEaogA))

Second example:
- broken: [https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBLAtgB1gJwC6gFSgCGAzqAEoCmRAxnpFrKqAORbV0uKI4Ce6lUAGUArgCMAjAAVG6MgF5QAb2IAuUADsRqMZSwAaUGPVade0AF8A3D36DRYgEwzYc0IpVF1JHFmQaAc0Mab19-AMsbRBpYDR9hcQl3UAAKdFkSdQdpDIBKdwA+UAAeABNkADdQYAKbGLi8B0dktIys8Wc8wpLyqpqo+viAYSYWpURQUBJKaEo6bH0J0AA6VfTXEkQLdRVp2fmsdV8RSkNV5eyXNwtQAB9lKZm5nGx1SCJoabPVpquyC3y8gKSz2z2woAA-CVsspzus5DcaqB1MUmrC1hlEbUgA)
- fixed: [https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBLAtgB1gJwC6gFSgCGAzqAEoCmRAxnpFrKqAORbV0uKI4Ce6lUAGUArgCMAjAAVG6MgF5QAbwA+xAFygAdiNRjKWADSgxmnXoOgVAXwDcPfoNFiATDNhzQi1RtAkcWMhaAObGNJr+gSFWdtw0sFr+wuISXqAAFOiyJJrO0tkAlF4AfKAAPAAmyABuoMDF9vGJeM4uaZnZueJuhSXlVbX19ohNSQDCTO1ZHjnKaiSU0JR02JoBIpTGAHQ7ee6eNlZzfovLOKugkETQC9u73ftkNkXypUqIoKDIkBnTclsLJYrLBFd6fT5LPBKQFnbB3LbsfzWNJ-Ej2cGgdg4ERYLTlPLKHYIyhIuoND6gZGLBbKCkQyhQmHA+GInDIxSo9HgrE4vFlVqEnas5FDCnWRDWIA)

### I got a "Missing type annotation" error. Where does it come from? <a class="toc" id="toc-i-got-a-missing-type-annotation-error-where-does-it-come-from" href="#toc-i-got-a-missing-type-annotation-error-where-does-it-come-from"></a>

Flow requires type annotations at module boundaries to make sure it can scale. To read more about that, check out our [blog post](https://medium.com/flow-type/asking-for-required-annotations-64d4f9c1edf8) about that.

The most common case you'll encounter is when exporting a function or React component. Flow requires you to annotate inputs. For instance, in this [example](https://flow.org/try/#0PTAEAEDMBsHsHcBQBTAHgB1gJwC6gMawB2AzngIYAmloAvKOXQHwOgDUoAjANxA), flow will complain:
```js
export const add = a => a + 1;
```

The fix here is to add types to the parameters of `add`.

Example ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBTAHgB1gJwC6gMawB2AzngIYAmloAvKABTkBcoRArgLYBGyWAlHQB8ocqADUoAIwBuIA)):
```js
export const add = (a: number) => a + 1;
```

To see how you can annotate exported React components, check out our docs on [HOCs](../react/hoc/#toc-exporting-wrapped-components).

There are other cases where this happens, and they might be harder to understand. You'll get an error like `Missing type annotation for U` For instance, you wrote this [code](https://flow.org/try/#0PTAEAEDMBsHsHcBQiDGsB2BnALqAhgE4F4CeoAvKANoDkeNANKDQEY0C6iApgB4AOsArjRZcAcy7ouBAJYoAgkVIV8SkgDoAtnj4AKPBQB8+AJRA):
```js
const array = ['a', 'b']
export const genericArray = array.map(a => a)
```

Here, Flow will complain on the `export`, asking for a type annotation. Flow wants you to annotate exports returned by a generic function. The type of `Array.prototype.map` is `map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>`. The `<U>` corresponds to what is called a [generic](https://flow.org/en/docs/types/generics/), to express the fact that the type of the function passed to map is linked to the type of the array.

Understanding the logic behind generics might be useful, but what you really need to know to make your typings valid is that you need to help Flow to understand the type of `genericArray`.

You can do that by adding an explicit type argument:

```js
const array = ['a', 'b'];
export const genericArray = array.map<string>(a => a);
```

or by annotating the exported constant ([https://flow.org/try](https://flow.org/try/#0PTAEAEDMBsHsHcBQBjWA7AzgF1AQwE764CeoAvKANoDku1ANKNQEbUC6iApgB4AOs+HKkw4A5pzSd8AS2QBBQiQBcoBUWIAebDLSiAfOTyLiAOgC2uXgApc5A7gCUQA)):
```js
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
