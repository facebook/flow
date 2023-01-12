---
title: Typing Generators with Flow
short-title: Generators
author: Sam Goldman
hide_table_of_contents: true
---

Flow 0.14.0 included support for generator functions. Generator functions provide a unique ability to JavaScript programs: the ability to suspend and resume execution. This kind of control paves the way for async/await, an [upcoming feature](https://github.com/tc39/ecmascript-asyncawait) already supported by Flow.

<!--truncate-->

So much wonderful material has already been produced describing generators. I am going to focus on the interaction of static typing with generators. Please refer to the following materials for information about generators:

* Jafar Husain gave an [incredibly lucid and well-illustrated talk](https://www.youtube.com/watch?v=DqMFX91ToLw#t=970) that covers generators. I have linked to the point where he gets into generators, but I highly recommend the entire talk.
* Exploring ES6, a comprehensive book by Axel Rauschmayer, who has generously made the contents available for free online, has a [chapter on generators](http://exploringjs.com/es6/ch_generators.html).
* The venerable MDN has a [useful page](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators) describing the `Iterator` interface and generators.

In Flow, the `Generator` interface has three type parameters: `Yield`, `Return`, and `Next`. `Yield` is the type of values which are yielded from the generator function. `Return` is the type of the value which is returned from the generator function. `Next` is the type of values which are passed into the generator via the `next` method on the `Generator` itself. For example, a generator value of type `Generator<string,number,boolean>` will yield `string`s, return a `number`, and will receive `boolean`s from its caller.

For any type `T`, a `Generator<T,void,void>` is both an `Iterable<T>` and an `Iterator<T>`.

The unique nature of generators allows us to represent infinite sequences naturally. Consider the infinite sequence of natural numbers:

```javascript
function *nats() {
  let i = 0;
  while (true) {
    yield i++;
  }
}
```

Because generators are also iterators, we can manually iterate the generator:

```javascript
const gen = nats();
console.log(gen.next()); // { done: false, value: 0 }
console.log(gen.next()); // { done: false, value: 1 }
console.log(gen.next()); // { done: false, value: 2 }
```

When `done` is false, `value` will have the generator's `Yield` type. When `done` is true, `value` will have the generator's `Return` type or `void` if the consumer iterates past the completion value.

```javascript
function *test() {
  yield 1;
  return "complete";
}
const gen = test();
console.log(gen.next()); // { done: false, value: 1 }
console.log(gen.next()); // { done: true, value: "complete" }
console.log(gen.next()); // { done: true, value: undefined }
```

Because of this behavior, manually iterating poses typing difficulties. Let's try to take the first 10 values from the `nats` generator through manual iteration:

```javascript
const gen = nats();
const take10: number[] = [];
for (let i = 0; i < 10; i++) {
  const { done, value } = gen.next();
  if (done) {
    break;
  } else {
    take10.push(value); // error!
  }
}
```

```
test.js:13
 13:   const { done, value } = gen.next();
                               ^^^^^^^^^^ call of method `next`
 17:     take10.push(value); // error!
                     ^^^^^ undefined. This type is incompatible with
 11: const take10: number[] = [];
                   ^^^^^^ number
```

Flow is complaining that `value` might be `undefined`. This is because the type of `value` is `Yield | Return | void`, which simplifies in the instance of `nats` to `number | void`. We can introduce a dynamic type test to convince Flow of the invariant that `value` will always be `number` when `done` is false.

```javascript
const gen = nats();
const take10: number[] = [];
for (let i = 0; i < 10; i++) {
  const { done, value } = gen.next();
  if (done) {
    break;
  } else {
    if (typeof value === "undefined") {
      throw new Error("`value` must be a number.");
    }
    take10.push(value); // no error
  }
}
```

There is an [open issue](https://github.com/facebook/flow/issues/577) which would make the dynamic type test above unnecessary, by using the `done` value as a sentinel to refine a tagged union. That is, when `done` is `true`, Flow would know that `value` is always of type `Yield` and otherwise of type `Return | void`.

Even without the dynamic type test, this code is quite verbose and it's hard to see the intent. Because generators are also iterable, we can also use `for...of` loops:

```javascript
const take10: number[] = [];
let i = 0;
for (let nat of nats()) {
  if (i === 10) break;
  take10.push(nat);
  i++;
}
```

That's much better. The `for...of` looping construct ignores completion values, so Flow understands that `nat` will always be `number`. Let's generalize this pattern further using generator functions:

```javascript
function *take<T>(n: number, xs: Iterable<T>): Iterable<T> {
  if (n <= 0) return;
  let i = 0;
  for (let x of xs) {
    yield x;
    if (++i === n) return;
  }
}

for (let n of take(10, nats())) {
  console.log(n); // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
}
```

Note that we explicitly annotated the parameters and return type of the `take` generator. This is necessary to ensure Flow understands the fully generic type. This is because Flow does not currently infer a fully generic type, but instead accumulates lower bounds, resulting in a union type.

```javascript
function identity(x) { return x }
var a: string = identity(""); // error
var b: number = identity(0);  // error
```

The above code produces errors because Flow adds `string` and `number` as lower bounds to the type variable describing the type of the value bound by `x`. That is, Flow believes the type of `identity` is `(x: string | number) => string | number` because those are the types which actually passed through the function.

Another important feature of generators is the ability to pass values into the generator from the consumer. Let's consider a generator `scan`, which reduces values passed into the generator using a provided function. Our `scan` is similar to `Array.prototype.reduce`, but it returns each intermediate value and the values are provided imperatively via `next`.

As a first pass, we might write this:

```javascript
function *scan<T,U>(init: U, f: (acc: U, x: T) => U): Generator<U,void,T> {
  let acc = init;
  while (true) {
    const next = yield acc;
    acc = f(acc, next);
  }
}
```

We can use this definition to implement an imperative sum procedure:

```javascript
let sum = scan(0, (a,b) => a + b);
console.log(sum.next());  // { done: false, value: 0 }
console.log(sum.next(1)); // { done: false, value: 1 }
console.log(sum.next(2)); // { done: false, value: 3 }
console.log(sum.next(3)); // { done: false, value: 6 }
```

However, when we try to check the above definition of `scan`, Flow complains:

```
test.js:7
  7:     acc = f(acc, next);
               ^^^^^^^^^^^^ function call
  7:     acc = f(acc, next);
                      ^^^^ undefined. This type is incompatible with
  3: function *scan<T,U>(init: U, f: (acc: U, x: T) => U): Generator<U,void,T> {
     ^ some incompatible instantiation of T
```

Flow is complaining that our value, `next`, may be `void` instead of the expected `T`, which is `number` in the `sum` example. This behavior is necessary to ensure type safety. In order to prime the generator, our consumer must first call `next` without an argument. To accommodate this, Flow understands the argument to `next` to be optional. This means Flow will allow the following code:

```javascript
let sum = scan(0, (a,b) => a + b);
console.log(sum.next());  // first call primes the generator
console.log(sum.next());  // we should pass a value, but don't need to
```

In general, Flow doesn't know which invocation is "first." While it should be an error to pass a value to the first `next`, and an error to *not* pass a value to subsequent `next`s, Flow compromises and forces your generator to deal with a potentially `void` value. In short, given a generator of type `Generator<Y,R,N>` and a value `x` of type `Y`, the type of the expression `yield x` is `N | void`.

We can update our definition to use a dynamic type test that enforces the non-`void` invariant at runtime:

```javascript
function *scan<T,U>(init: U, f: (acc: U, x: T) => U): Generator<U,void,T> {
  let acc = init;
  while (true) {
    const next = yield acc;
    if (typeof next === "undefined") {
      throw new Error("Caller must provide an argument to `next`.");
    }
    acc = f(acc, next);
  }
}
```

There is one more important caveat when dealing with typed generators. Every value yielded from the generator must be described by a single type. Similarly, every value passed to the generator via `next` must be described by a single type.

Consider the following generator:

```javascript
function *foo() {
  yield 0;
  yield "";
}

const gen = foo();
const a: number = gen.next().value; // error
const b: string = gen.next().value; // error
```

This is perfectly legal JavaScript and the values `a` and `b` do have the correct types at runtime. However, Flow rejects this program. Our generator's `Yield` type parameter has a concrete type of `number | string`. The `value` property of the iterator result object has the type `number | string | void`.

We can observe similar behavior for values passed into the generator:

```
function *bar() {
  var a = yield;
  var b = yield;
  return {a,b};
}

const gen = bar();
gen.next(); // prime the generator
gen.next(0);
const ret: { a: number, b: string } = gen.next("").value; // error
```

The value `ret` has the annotated type at runtime, but Flow also rejects this program. Our generator's `Next` type parameter has a concrete type of `number | string`. The `value` property of the iterator result object thus has the type `void | { a: void | number | string, b: void | number | string }`.

While it may be possible to use dynamic type tests to resolve these issues, another practical option is to use `any` to take on the type safety responsibility yourself.

```javascript
function *bar(): Generator {
  var a = yield;
  var b = yield;
  return {a,b};
}

const gen = bar();
gen.next(); // prime the generator
gen.next(0);
const ret: void | { a: number, b: string } = gen.next("").value; // OK
```

(Note that the annotation `Generator` is equivalent to `Generator<any,any,any>`.)

Phew! I hope that this will help you use generators in your own code. I also hope this gave you a little insight into the difficulties of applying static analysis to a highly dynamic language such as JavaScript.

To summarize, here are some of the lessons we've learned for using generators in statically typed JS:

* Use generators to implement custom iterables.
* Use dynamic type tests to unpack the optional return type of yield expressions.
* Avoid generators that yield or receive values of multiple types, or use `any`.
