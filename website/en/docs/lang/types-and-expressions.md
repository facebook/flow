---
layout: guide
---

In JavaScript there are many types of values: numbers, strings, booleans,
functions, objects, and more.

```js
(1234: number);
("hi": string);
(true: boolean);
([1, 2]: Array<number>);
({ prop: "value" }: Object);
(function method() {}: Function);
```

These values can be used in many different ways:

```js
1 + 2;
"foo" + "bar";
!true;
[1, 2].push(3);
let value = obj.prop;
obj.prop = "value";
method("value");
```

All of these different expressions create a new type which is a result of the
types of values and the operations run on them.

```js
let num: number = 1 + 2;
let str: string = "foo" + "bar";
```

In Flow every value and expression has a type.

## Figuring out types statically <a class="toc" id="toc-figuring-out-types-statically" href="#toc-figuring-out-types-statically"></a>

Flow needs a way to be able to figure out the type of every expression. But it
can't just run your code to figure it out, if it did it would be affected by
any issues that your code has. For example, if you created an infinite loop
Flow would wait for it to finish forever.

Instead, Flow needs to be able to figure out the type of a value by analyzing
it without running it (static analysis). It works its way through every known
type and starts to figure out what all the expressions around them result in.

For example, to figure out the result of following expression Flow needs to
figure out what the values are in it first.

```js
val1 + val2;
```

If the values are numbers, then the expression results in a number. If the
values are strings, then the expression results in a string. There are a number
of different possibilities here, so Flow must look up what the values are.

If Flow is unable to figure out what the exact type is for each value, Flow
must figure out what every possible value is and check to make sure that the
code around it will still work with all of the possible types.

## Soundness and Completeness <a class="toc" id="toc-soundness-and-completeness" href="#toc-soundness-and-completeness"></a>

When you run your code, a single expression will only be run with a limited set
of values. But still Flow checks _every_ possible value. In this way Flow is
checking too many things or _over-approximating_ what will be valid code.

By checking every possible value, Flow might catch errors that will not
actually occur when the code is run. Flow does this in order to be _"sound"_.

In type systems, ***soundness*** is the ability for a type checker to catch
every single error that _might_ happen at runtime. This comes at the cost of
sometimes catching errors that will not actually happen at runtime.

On the flip-side, ***completeness*** is the ability for a type checker to only
ever catch errors that _would_ happen at runtime. This comes at the cost of
sometimes missing errors that will happen at runtime.

In an ideal world, every type checker would be both sound _and_ complete so
that it catches _every_ error that _will_ happen at runtime.

Flow tries to be as sound and complete as possible. But because JavaScript was
not designed around a type system, Flow sometimes has to make a tradeoff. When
this happens Flow tends to favor soundness over completeness, ensuring that
code doesn't have any bugs.

Soundness is fine as long as Flow isn't being too noisy and preventing you from
being productive. Sometimes when soundness would get in your way too much, Flow
will favor completeness instead. There's only a handful of cases where Flow
does this.

Other type systems will favor completeness instead, only reporting real errors
in favor of possibly missing errors. Unit/Integration testing is an extreme
form of this approach. Often this comes at the cost of missing the errors that
are the most complicated to find, leaving that part up to the developer.
