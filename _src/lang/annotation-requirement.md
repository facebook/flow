---
title: Annotation Requirement
slug: /lang/annotation-requirement
---

> **Note:** As of version 0.199 Flow uses [Local Type Inference](https://medium.com/flow-type/local-type-inference-for-flow-aaa65d071347) as its inference algorithm.
The rules in this section reflect the main design features in this inference scheme.

Flow tries to avoid requiring type annotation for parts of programs where types can easily
be inferred from the immediate context of an expression, variable, parameter, etc.

## Variable declarations

Take for example the following variable definition
```js
const len = "abc".length;
```
All information necessary to infer the type of `len` is included in the initializer
`"abc".length`. Flow will first determine that `"abc"` is a string, and then that the
`length` property of a string is a number.

The same logic can be applied for all `const`-like initializations. Where things
get a little more complicated is when variable initialization spans across multiple statements,
for example in
```js flow-check
declare const maybeString: ?string;

let len;
if (typeof maybeString === "string") {
  len = maybeString.length;
} else {
  len = 0;
}
```
Flow can still determine that `len` is a `number`, but in order to do so it looks
ahead to multiple initializer statements. See section on [variable declarations](../variables)
for details on how various initializer patterns determine the type of a variable,
and when an annotation on a variable declaration is necessary.

## Function Parameters

Unlike variable declarations, this kind of "lookahead" reasoning cannot be used to determine
the type of function parameters. Consider the function
```js
function getLength(x) {
  return x.length;
}
```
There are many kinds of `x` on which we could access and return a `length` property:
an object with a `length` property, or a string, just to name a few. If later on in
the program we had the following calls to `getLength`
```js
getLength("abc");
getLength({length: 1});
```
one possible inference would be that `x` is a `string | { length: number }`. What this implies,
however, is that the type of `getLength` is determined by any part of the current
program. This kind of global reasoning can lead to surprising action-at-a-distance
behavior, and so is avoided. Instead, Flow requires that function parameters are annotated. Failure to
provide such a type annotation manifests as a `[missing-local-annot]` error on the parameter `x`,
and the body of the function is checked with `x: any`:
```js flow-check
function getLength(x) {
  return x.length;
}

const n = getLength(1); // no error since getLength's parameter type is 'any'
```

To fix this error, one can simply annotate `x` as
```js flow-check
function getLength(x: string) {
  return x.length;
}
```
The same requirement holds for class methods
```js flow-check
class WrappedString {
  data: string;
  setStringNoAnnotation(x) {
    this.data = x;
  }
  setString(x: string) {
    this.data = x;
  }
}
```

## Contextual Typing {#toc-contextual-typing}

Function parameters do not always need to be explicitly annotated. In the case of a
callback function to a function call, the parameter type can easily
be inferred from the immediate context. Consider for example the following code
```js
const arr = [0, 1, 2];
const arrPlusOne = arr.find(x => x % 2 === 1);
```
Flow infers that the type of `arr` is `Array<number>`. Combining this with the builtin
information for `Array.find`, Flow can determine that the type of `x => x % 2 + 1`
needs to be `number => mixed`. This type acts as a *hint* for Flow and provides enough
information to determine the type of `x` as `number`.

Any attendant annotation can potentially act as a hint to a function parameter, for example
```js flow-check
const fn1: (x: number) => number = x => x + 1;
```
However, it is also possible that an annotation cannot be used as a function
parameter hint:
```js flow-check
const fn2: mixed = x => x + 1;
```
In this example the `mixed` type simply does not include enough information to
extract a candidate type for `x`.

Flow can infer the types for unannotated parameters even when they are nested within
other expressions like objects. For example in
in
```js flow-check
const fn3: {f: (number) => void} = {f: (x) => {x as string}};
```
Flow will infer `number` as the type of `x`, and so the cast fails.


## Function Return Types {#toc-function-return-types}

Unlike function parameters, a function's return type does not need to be annotated in general.
So the above definition of `getLength` won't raise any Flow errors.

There are, however, a couple of notable exceptions to this rule. The first one is
class methods. If we included to the `WrappedString` class a `getString` method
that returns the internal `data` property:
```js flow-check
class WrappedString {
  data: string;
  getString(x: string) {
    return this.data;
  }
}
```
Flow would complain that `getString` is missing an annotation on the return.

The second exception is recursive definitions. A trivial example of this would be
```js flow-check
function foo() {
  return bar();
}

function bar() {
  return foo();
}
```
The above code raises a `[definition-cycle]` error, which points to the two locations
that form a dependency cycle, the two missing return annotations. Adding
a return annotation to either function would resolve the issue.

Effectively, the requirement on an annotation for method returns is a special-case
of the recursive definition restriction. The recursion is possible through access on
`this`.

## Generic Calls {#toc-generic-calls}

In calls to [generic functions](../../types/generics) the type of the result may
depend on the types of the values passed in as arguments.
This section discusses how this result is computed, when type arguments are not
explicitly provided.

Consider for example the definition
```js
declare function map<T, U>(
  f: (T) => U,
  array: $ReadOnlyArray<T>,
): Array<U>;
```
and a potential call with arguments `x => x + 1` and `[1, 2, 3]`:
```js
map(x => x + 1, [1, 2, 3]);
```
Here Flow infers that the type of `x` is `number`.

Some other common examples of generic calls are calling the constructor of the generic
[`Set` class](https://github.com/facebook/flow/blob/82f88520f2bfe0fa13748b5ead711432941f4cb9/lib/core.js#L1799-L1801)
or calling `useState` from the React library:
```js flow-check
const set = new Set([1, 2, 3]);

import {useState} from 'react';
const [num, setNum] = useState(42);
```
Flow here infers that the type of `set` is `Set<number>`, and that `num` and `setNum`
are `number` and `(number) => void`, respectively.

### Computing a Solution

Computing the result of a generic call amounts to:
1. coming up with a solution for `T` and `U` that does not contain generic parts,
2. replacing `T` and `U` with the solution in the signature of `map`, and
3. performing a call to this new signature of `map`.

This process is designed with two goals in mind:
* *Soundness*. The results need to lead to a correct call when we reach step (3).
* *Completeness*. The types Flow produces need to be as precise and informative as possible,
to ensure that other parts of the program will be successfully checked.

Let's see how these two goals come into play in the `map` example from above.

Flow detects that `$ReadOnlyArray<T>` needs to be compatible with the type of `[1, 2, 3]`.
It can therefore infer that `T` is `number`.

With the knowledge of `T` it can now successfully check `x => x + 1`. The parameter `x`
is contextually typed as `number`, and thus the result `x + 1` is also a number.
This final constraint allows us to compute `U` as a `number` too.

The new signature of `map` after replacing the generic parts with the above solution
is
```js
(f: (number) => number, array: $ReadOnlyArray<number>) => Array<number>
```
It is easy to see that the call would be successfully checked.

### Errors during Polymorphic Calls

If the above process goes on smoothly, you should not be seeing any errors associated with the call.
What happens though when this process fails?

There are two reasons why this process could fail:

#### Under-constrained Type Parameters

There are cases where Flow might not have enough information to decide the type of a type parameter.
Let's examine again a call to the builtin generic
[`Set` class](https://github.com/facebook/flow/blob/82f88520f2bfe0fa13748b5ead711432941f4cb9/lib/core.js#L1799-L1801)
constructor, this time without passing any arguments:
```js flow-check
const set = new Set();
set.add("abc");
```
During the call to `new Set`, we are not providing enough information for Flow to
determine the type for `T`, even though the subsequent call to `set.add` clearly
implies that `T` will be a string. Remember that inference of type arguments is
local to the call, so Flow will not attempt to look ahead in later statements
to determine this.

In the absence of information, Flow would be at liberty to infer *any* type
as `T`: `any`, `mixed`, `empty`, etc.
This kind of decision is undesirable, as it can lead to surprising results.
For example, if we silently decided on `Set<empty>` then the call to `set.add("abc")` would
fail with an incompatibility between `string` and `empty`, without a clear indication
of where the `empty` came from.

So instead, in situations like this, you'll get an `[underconstrained-implicit-instantiation]` error.
The way to fix this error is by adding a type annotation. There a few potential ways to do this:

- Add an annotation at the call-site in one of two ways:
  * an explicit type argument
    ```js
    const set = new Set<string>();
    ```
  * an annotation on the initialization variable:
    ```js
    const set: Set<string> = new Set();
    ```

- Add a default type on the type parameter `T` at the definition of the class:
    ```js
    declare class SetWithDefault<T = string> extends $ReadOnlySet<T> {
      constructor(iterable?: ?Iterable<T>): void;
      // more methods ...
    }
    ```
  In the absence of any type information at the call-site, Flow will use the default
  type of `T` as the inferred type argument:
  ```js
  const defaultSet = new SetWithDefault(); // defaultSet is SetWithDefault<string>
  ```

#### Incompatibility Errors

Even when Flow manages to infer non-generic types for the type parameters in a generic
call, these types might still lead to incompatibilities either in the current call or in
code later on.

For example, if we had the following call to `map`:
```js flow-check
declare function map<T, U>(f: (T) => U, array: $ReadOnlyArray<T>): Array<U>;
map(x => x + 1, [{}]);
```
Flow will infer `T` as `{}`, and therefore type `x` as `{}`. This will cause an error when checking the arrow function
since the `+` operation is not allowed on objects.

Finally, a common source of errors is the case where the inferred type in a generic
call is correct for the call itself, but not indicative of the expected use later in the code.
For example, consider
```js flow-check
import {useState} from 'react';
const [str, setStr] = useState("");

declare const maybeString: ?string;
setStr(maybeString);
```
Passing the string `""` to the call to `useState` makes Flow infer `string` as the type
of the state. So `setStr` will also expect a `string` as input when called later on,
and therefore passing a `?string` will be an error.

Again, to fix this error it suffices to annotate the expected "wider" type of state
when calling `useState`:
```js
const [str, setStr] = useState<?string>("");
```

## Empty Array Literals {#toc-empty-array-literals}
Empty array literals (`[]`) are handled specially in Flow. You can read about their [behavior and requirements](../../types/arrays/#toc-empty-array-literals).
