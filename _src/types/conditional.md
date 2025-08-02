---
title: Conditional Types
slug: /types/conditional
---

Flow's conditional type allows you to conditionally choose between two different output types by inspecting an input type. It is useful to extract parts of a type, or to describe a complex overload.

## Basic Usage {#toc-basic-usage}

It has a syntax that is similar to conditional expressions: `CheckType extends ExtendsType ? TrueType : FalseType`.

If `CheckType` is a subtype of `ExtendsType`, the conditional type will be evaluated to `TrueType`. Otherwise, it will be evaluated to `FalseType`.

The following example illustrates both cases.

```js flow-check
class Animal {}
class Dog extends Animal {}

type TypeofAnimal = Dog extends Animal ? 'animal' : 'unknown'; // evaluates to 'animal'
type TypeofString = string extends Animal ? 'animal' : 'unknown'; // evaluates to 'unknown'
```

## Generic Conditional Types {#toc-generic-conditional-types}

This might not look very useful, since you already know what type it will evaluate to. However, combining with generics, you can perform complex computations over types. For example, you can write down a type-level `typeof` operator:

```js flow-check
type TypeOf<T> =
  T extends null ? 'null' :
  T extends void ? 'undefined' :
  T extends string ? 'string' :
  T extends number ? 'number' :
  T extends boolean ? 'boolean' :
  T extends (...$ReadOnlyArray<empty>)=>mixed ? 'function' : 'object'

type T1 = TypeOf<null>; // evaluates to 'null'
type T2 = TypeOf<void>; // evaluates to 'undefined'
type T3 = TypeOf<string>; // evaluates to 'string'
type T4 = TypeOf<number>; // evaluates to 'number'
type T5 = TypeOf<boolean>; // evaluates to 'boolean'
type T6 = TypeOf<(string)=>boolean>; // evaluates to 'function'
type T7 = TypeOf<{foo: string}>; // evaluates to 'object'
```

## Function return types dependent on input types {#toc-dependent-function-return}

Conditional types also allow you to intuitively describe the conditions for choosing different function overloads:

```js flow-check
declare function wrap<T>(value: T): T extends string ? { type: 'string', value: string }
                                  : T extends number ? { type: 'number', value: number }
                                  : { type: 'unsupported' }

const v1 = wrap(3);   // has type { type: 'number', value: number }
const v2 = wrap('4'); // has type { type: 'string', value: string }
const v3 = wrap({});  // has type { type: 'unsupported' }
```

The above example can also be written with function overload:

```js flow-check
declare function wrap(value: string): { type: 'string', value: string }
declare function wrap(value: number): { type: 'number', value: number }
declare function wrap(value: mixed): { type: 'unsupported' }

const v1 = wrap(3);   // has type { type: 'number', value: number }
const v2 = wrap('4'); // has type { type: 'string', value: string }
const v3 = wrap({});  // has type { type: 'unsupported' }
```

## Inferring Within Conditional Types {#toc-infer-type}

You can use the power of conditional types to extract parts of a type using `infer` types. For example, the builtin `ReturnType` is powered by conditional types:

```js flow-check
type ReturnType<T> = T extends (...args: $ReadOnlyArray<empty>) => infer Return ? Return : empty;

type N = ReturnType<(string) => number>; // evaluates to `number`
type S = ReturnType<(number) => string>; // evaluates to `string`
```

We used the infer type here to introduce a new generic type variable named `Return`, which can be used in the type branch of the conditional type. Infer types can only appear on the right hand side of the `extends` clause in conditional types. Flow will perform a subtyping check between the check type and the extends type to automatically figure out its type based on the input type `T`.

In the example of `type N = ReturnType<(string) => number>`, Flow checks if `(string) => number` is a subtype of `(...args: $ReadOnlyArray<empty>) => infer Return`, and during this process `Return` is constrained to `number`.

When doing extractions like the above example, you usually want the conditional type to always choose the true branch where the type is successfully extracted. For example, silently choosing the false branch is not great:

```js flow-check
type ExtractReturnTypeNoValidation<T> =
  T extends (...args: $ReadOnlyArray<empty>) => infer Return ? Return : any;

1 as ExtractReturnTypeNoValidation<string>; // no error :(
```

Instead, you might want Flow to error when the input is not a function type. This can be accomplished by adding constraints to the type parameter:

```js flow-check
type ReturnType<T: (...args: $ReadOnlyArray<empty>) => mixed> =
  T extends (...args: $ReadOnlyArray<empty>) => infer Return ? Return : any;

1 as ReturnType<(string) => number>;
1 as ReturnType<string>;
```

## Distributive Conditional Types {#toc-distributive-conditional-type}

When a generic conditional type is given a union type as a type argument, the conditional _distributes_ over the union's members. For example, the `TypeOf` example above can distribute over a union:

```js flow-check
type TypeOf<T> =
  T extends null ? 'null' :
  T extends void ? 'undefined' :
  T extends string ? 'string' : 'other';

type StringOrNull = TypeOf<string | null>; // evaluates to 'string' | 'null'
```

This works by first breaking up the union type, and then passing each type to the conditional type to be evaluated separately. In the example above, this looks something like:

```
TypeOf<string | null>
--> (break up the union) --> TypeOf<string> | TypeOf<null>
--> (evaluate each conditional type separately) --> 'string' | 'null'
```

If you want to avoid this behavior, you can wrap the check type and extends type with unary tuple type:

```js flow-check
type NonDistributiveTypeOf<T> =
  [T] extends [null] ? 'null' :
  [T] extends [void] ? 'undefined' :
  [T] extends [string] ? 'string' : 'other';

type Other = NonDistributiveTypeOf<string | null>; // evaluates to 'other'
```

This trick works because Flow will only enable the distributive behavior of conditional type if the check type is a generic type. The example above does not choose any true branch of the conditional type, because `[string | null]` is not a subtype of `[null]`, `[void]`, or `[string]`, since tuples are [invariantly](../../lang/variance/#toc-invariance) typed.

## Adoption {#toc-adoption}

To use conditional types, you need to upgrade your infrastructure so that it supports the syntax:

- `flow` and `flow-parser`: 0.208.0. Between v0.208 to v0.211.1, you need to explicitly enable it in your .flowconfig, under the `[options]` heading, add `conditional_type=true`.
- `prettier`: 3
- `babel` with `babel-plugin-syntax-hermes-parser`. See [our Babel guide](../../tools/babel/) for setup instructions.
- `eslint` with `hermes-eslint`. See [our ESLint guide](../../tools/eslint/) for setup instructions.
