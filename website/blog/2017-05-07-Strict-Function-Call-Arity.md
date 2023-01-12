---
title: "Strict Checking of Function Call Arity"
short-title: "Strict Function Call Arity"
author: "Gabe Levi"
hide_table_of_contents: true
---

One of Flow's original goals was to be able to understand idiomatic JavaScript.
In JavaScript, you can call a function with more arguments than the function
expects. Therefore, Flow never complained about calling a function with
extraneous arguments.

We are changing this behavior.

<!--truncate-->

### What is arity?

A function's *arity* is the number of arguments it expects. Since some functions
have optional parameters and some use rest parameters, we can define the
*minimum arity* as the smallest number of arguments it expects and the *maximum
arity* as the largest number of arguments it expects.

```js
function no_args() {} // arity of 0
function two_args(a, b) {} // arity of 2
function optional_args(a, b?) {} // min arity of 1, max arity of 2
function many_args(a, ...rest) {} // min arity of 1, no max arity
```

### Motivation

Consider the following code:

```js
function add(a, b) { return a + b; }
const sum = add(1, 1, 1, 1);
```

The author apparently thought the `add()` function adds up all its
arguments, and that `sum` will have the value `4`. However, only the first two
arguments are summed, and `sum` actually will have the value `2`. This is
obviously a bug, so why doesn't JavaScript or Flow complain?

And while the error in the above example is easy to see, in real code it's often
a lot harder to notice. For example, what is the value of `total` here:

```js
const total = parseInt("10", 2) + parseFloat("10.1", 2);
```

`"10"` in base 2 is `2` in decimal and `"10.1"` in base 2 is `2.5` in decimal.
So the author probably thought that `total` would be `4.5`. However, the correct
answer is `12.1`. `parseInt("10", 2)` does evaluates to `2`, as expected.
However, `parseFloat("10.1", 2)` evaluates to `10.1`. `parseFloat()` only takes
a single argument. The second argument is ignored!

### Why JavaScript allows extraneous arguments

At this point, you might feel like this is just an example of JavaScript making
terrible life decisions. However, this behavior is very convenient in a bunch of
situations!

#### Callbacks

If you couldn't call a function with more arguments than it handles, then
mapping over an array would look like

```js
const doubled_arr = [1, 2, 3].map((element, index, arr) => element * 2);
```

When you call `Array.prototype.map`, you pass in a callback. For each element in
the array, that callback is invoked and passed 3 arguments:

1. The element
2. The index of the element
3. The array over which you're mapping

However, your callback often only needs to reference the first argument: the
element. It's really nice that you can write

```js
const doubled_arr = [1, 2, 3].map(element => element * 2);
```

#### Stubbing

Sometimes I come across code like this

```js
let log = () => {};
if (DEBUG) {
  log = (message) => console.log(message);
}
log("Hello world");
```

The idea is that in a development environment, calling `log()` will output a
message, but in production it does nothing. Since you can call a
function with more arguments than it expects, it is easy to stub out `log()` in
production.

#### Variadic functions using `arguments`

A variadic function is a function that can take an indefinite number of
arguments. The old-school way to write variadic functions in JavaScript is by
using `arguments`. For example

```js
function sum_all() {
  let ret = 0;
  for (let i = 0; i < arguments.length; i++) { ret += arguments[i]; }
  return ret;
}
const total = sum_all(1, 2, 3); // returns 6
```

For all intents and purposes, `sum_all` appears like it takes no arguments. So
even though it appears to have an arity of 0, it is convenient that we can call
it with more arguments.

### Changes to Flow

We think we have found a compromise which catches the motivating bugs without
breaking the convenience of JavaScript.

#### Calling a function

If a function has a maximum arity of N, then Flow will start complaining if you
call it with more than N arguments.

```js
test:1
  1: const num = parseFloat("10.5", 2);
                                    ^ unused function argument
   19: declare function parseFloat(string: mixed): number;
                                  ^^^^^^^^^^^^^^^^^^^^^^^ function type expects no more than 1 argument. See lib: <BUILTINS>/core.js:19
```

#### Function subtyping

Flow will not change its function subtyping behavior. A function
with a smaller maximum arity is still a subtype of a function with a larger
maximum arity. This allows callbacks to still work as before.

```js
class Array<T> {
  ...
  map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>;
  ...
}
const arr = [1,2,3].map(() => 4); // No error, evaluates to [4,4,4]
```

In this example, `() => number` is a subtype of `(number, number, Array<number>) => number`.

#### Stubbing and variadic functions

This will, unfortunately, cause Flow to complain about stubs and variadic
functions which are written using `arguments`. However, you can fix these by
using rest parameters

```js
let log (...rest) => {};

function sum_all(...rest) {
  let ret = 0;
  for (let i = 0; i < rest.length; i++) { ret += rest[i]; }
  return ret;
}
```

### Rollout plan

Flow v0.46.0 will ship with strict function call arity turned off by default. It
can be enabled via your `.flowconfig` with the flag

```ini
experimental.strict_call_arity=true
```

Flow v0.47.0 will ship with strict function call arity turned on and the
`experimental.strict_call_arity` flag will be removed.


#### Why turn this on over two releases?

This decouples the switch to strict checking of function call arity from the
release.

#### Why not keep the `experimental.strict_call_arity` flag?

This is a pretty core change. If we kept both behaviors, we'd have to test that
everything works with and without this change. As we add more flags, the number
of combinations grows exponentially, and Flow's behavior gets harder to reason
about. For this reason, we're choosing only one behavior: strict checking of
function call arity.

### What do you think?

This change was motivated by feedback from Flow users. We really appreciate
all the members of our community who take the time to share their feedback with
us. This feedback is invaluable and helps us make Flow better, so please keep
it coming!
