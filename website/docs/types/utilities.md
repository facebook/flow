---
title: Utility Types
slug: /types/utilities
---

import {SinceVersion, UntilVersion} from '../../components/VersionTags';

Flow provides a set of utility types to operate on other types to create new types.

## `$Keys<T>` {#toc-keys}

You can extract the type of the keys from an [object type](../objects). Typically this will be a [union](../unions) of [string literal](../literals) types:

```js flow-check
const countries = {
  US: "United States",
  IT: "Italy",
  FR: "France"
};

type Country = $Keys<typeof countries>;

const italy: Country = 'IT'; // Works
const nope: Country = 'nope'; // Error!
```

In the example above, the type of `Country` is equivalent to `type Country = 'US' | 'IT' | 'FR'`, but Flow was able to extract it from the keys of `countries`.

If you want to create an enum type, [Flow Enums](../../enums) might be a better fit for your use-case.

## `$Values<T>` {#toc-values}

`$Values<T>` represents the union type of all the value types of the enumerable properties in an [object type](../objects/):

```js flow-check
type Props = {
  name: string,
  age: number,
};

// The following two types are equivalent:
type PropValues = string | number;
type Prop$Values = $Values<Props>;

const name: Prop$Values = 'Jon';  // Works
const age: Prop$Values = 42;  // Works
const fn: Prop$Values = true; // Error!
```

Note that using `$Values` on the [`typeof`](../typeof) an object literal will result in a type more general than you might expect:

```js flow-check
const obj = {
  foo: 1,
  bar: 2,
};

function acceptsValues(x: $Values<typeof obj>) { /* ... */ }

acceptsValues(1); // Works
acceptsValues(3); // Works, because the type was interpreted as `number`.
```

This behavior changes if you use `Object.freeze` on the object literal expression:

```js flow-check
const obj = Object.freeze({
  foo: 1,
  bar: 2,
});

function acceptsValues(x: $Values<typeof obj>) { /* ... */ }

acceptsValues(1); // Works
acceptsValues(3); // Error! Because the type was interpreted as `1 | 2`.
```

If you want to create an enum type, [Flow Enums](../../enums) might be a better fit for your use-case.

## `$ReadOnly<T>` {#toc-readonly}

`$ReadOnly<T>` is a type that represents the read-only version of a given [object type](../objects/) `T`.
A read-only object type is an object type whose keys are all [read-only](../objects/#read-only-object-properties).

This means that the following two types are equivalent:
```js flow-check
type ReadOnlyObj = {
  +key: number,  // read-only field, marked by the `+` annotation
};
```
```js flow-check
type ReadOnlyObj = $ReadOnly<{
  key: number,
}>;
```

This is useful when you need to use a read-only version of an object type you've already defined, without manually having to re-define and annotate each key as read-only. For example:

```js flow-check
type Props = {
  name: string,
  age: number,
};

type ReadOnlyProps = $ReadOnly<Props>;

function render(props: ReadOnlyProps) {
  const {name, age} = props;  // Works
  props.age = 42;             // Error!
}
```

Additionally, other utility types, such as [`$ObjMap<T>`](#toc-objmap), may strip any read/write annotations, so `$ReadOnly<T>` is a handy way to quickly make the object read-only again after operating on it:

```js flow-check
type Obj = {
  +key: number,
};

type MappedObj = $ReadOnly<$ObjMap<Obj, <T>(T) => Array<T>>> // Still read-only
```

The `$ReadOnly` utility works on object types.
If you want to make other types read-only, you can use one of the following:
- `Array<T>` -> [`$ReadOnlyArray<T>`](../arrays/#toc-readonlyarray)
- `Set<T>` -> `$ReadOnlySet<T>`
- `Map<K, V>` -> `$ReadOnlyMap<K, V>`
- `WeakSet<T>` -> `$ReadOnlyWeakSet<T>`
- `WeakMap<K, V>` -> `$ReadOnlyWeakMap<K, V>`

## `Partial<T>`  <SinceVersion version="0.201" /> {#toc-partial}

This utility converts all of an object or interface's named fields to be optional,
while maintaining all the object's other properties (e.g. exactness, variance).
Use this utility instead of `$Shape`.

```js flow-check
type Person = {
  name: string,
  age: number,
};
type PartialPerson = Partial<Person>;
// Above equivalent to `{name?: string, age?: number}`

const a: PartialPerson = {}; // OK
const b: PartialPerson = {name: 'George'}; // OK
const c: PartialPerson = {name: 'George', age: 123}; // OK

(c: Person); // ERROR: `PersonDetails` is not a `Person` (unlike with `$Shape`)
```

A object of type `T` cannot be supplied to `Partial<T>`, due to mutability. You can resolve this by making the object [read-only](#toc-readonly):

```js flow-check
type Person = {
  name: string,
  age: number,
};

const person: Person = {name: 'George', age: 123};

function noPerson(o: Partial<Person>) {
  // Can mutate:
  o.name = undefined;
}
noPerson(person); // Error!

function okPerson(o: $ReadOnly<Partial<Person>>) {
  // Can't mutate - it's read-only!
}
okPerson(person); // Works
```

Note: Up until Flow version 0.201, this utility type was named `$Partial`.

## `Required<T>` <SinceVersion version="0.201" /> {#toc-required}

The `Required` utility type is the opposite of [`Partial`](#toc-partial):
it converts all of an object or interface’s optional fields to be required. For example:

```js flow-check
type PartialPerson = {
  name?: string,
  age?: number,
};
type Person = Required<PartialPerson>;
// Above equivalent to `{name: string, age: number}`

const a: Person = {name: 'George', age: 123}; // OK
const b: Person = {age: 123}; // ERROR: missing `name` property
```


## `$Exact<T>` {#toc-exact}

You can use `$Exact` to make an [inexact object type](../objects/#exact-and-inexact-object-types) exact:

```js flow-check
type InexactUser = {name: string, ...};
type ExactUser = $Exact<InexactUser>;

const user = {name: 'John Wilkes Booth'};
// These will both be satisfied because they are equivalent:
const a: ExactUser = user;
const b: {name: string} = user;
```

This is an utility type to avoid, as it's clearer and more concinse to start off with an exact object type and make it inexact using [object type spread](../objects/#object-type-spread)
(if you wish to have both inexact and exact variants of one object type):

```js flow-check
type ExactUser = {name: string};
type InexactUser = {...ExactUser, ...};

const user = {name: 'John Wilkes Booth'};
const a: ExactUser = user;
```

## `$Diff<A, B>` {#toc-diff}

As the name hints, `$Diff<A, B>` is the type representing the set difference of `A` and `B`, i.e. `A \ B`, where `A` and `B` are both [object types](../objects/). Here's an example:

```js flow-check
type Props = {name: string, age: number, ...};
type DefaultProps = {age: number};
type RequiredProps = $Diff<Props, DefaultProps>;

function setProps(props: RequiredProps) {
  // ...
}

setProps({name: 'foo'}); // Works
setProps({name: 'foo', age: 42, baz: false}); // Works, you can pass extra props too
setProps({age: 42}); // Error! `name` is required
```

As you may have noticed, the example is not a random one.
`$Diff` is exactly what the React definition file uses to define the type of the props accepted by a React Component.

Note that `$Diff<A, B>` will error if the object you are removing properties from does not have the property being removed, i.e. if `B` has a key that doesn't exist in `A`:

```js flow-check
type Props = {name: string, age: number};
type DefaultProps = {age: number, other: string};
type RequiredProps = $Diff<Props, DefaultProps>; // Error!

function setProps(props: RequiredProps) {
  props.name;
  // ...
}
```

As a workaround, you can specify the property not present in `A` as optional. For example:

```js flow-check
type A = $Diff<{}, {nope: number}>; // Error!
type B = $Diff<{}, {nope: number | void}>; // Works

const a: A = {};
const b: B = {};
```

## `$Rest<A, B>` {#toc-rest}

`$Rest<A, B>` is the type that represents the runtime object rest operation, e.g.: `const {foo, ...rest} = obj`, where `A` and `B` are both [object types](../objects/).
The resulting type from this operation will be an object type containing `A`'s *own* properties that are not *own* properties in `B`.
In flow, we treat all properties on [exact object types](../objects/#exact-and-inexact-object-types) as [own](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwnProperty).
For inexact objects, a property may or may not be own.

For example:

```js flow-check
type Props = {name: string, age: number};

const props: Props = {name: 'Jon', age: 42};
const {age, ...otherProps} = props;
(otherProps: $Rest<Props, {age: number}>);
otherProps.age;  // Error!
```

The main difference with [`$Diff<A, B>`](#toc-diff), is that `$Rest<A, B>` aims to represent the true runtime rest operation,
which implies that exact object types are treated differently in `$Rest<A, B>`.
For example, `$Rest<{n: number}, {...}>` will result in `{n?: number}` because an in-exact empty object may have an `n` property,
while `$Diff<{n: number}, {...}>` will result in `{n: number}`.

## `$NonMaybeType<T>` {#toc-nonmaybe}

`$NonMaybeType<T>` converts a type `T` to a non-[maybe type](../maybe).
In other words, the values of `$NonMaybeType<T>` are the values of `T` except for `null` and `undefined`.

```js flow-check
type MaybeName = ?string;
type Name = $NonMaybeType<MaybeName>;

('Gabriel': MaybeName); // Works
(null: MaybeName); // Works
('Gabriel': Name); // Works
(null: Name); // Error! `null` can't be annotated as Name because Name is not a maybe type
```

## `$ObjMap<T, F>` {#toc-objmap}

`ObjMap<T, F>` takes an [object type](../objects) `T`, and a [function type](../functions) `F`, and returns the object type obtained by mapping the type of each value in the object with the provided function type `F`. In other words, `$ObjMap` will [call](#toc-call) (at the type level) the given function type `F` for every property value type in `T`, and return the resulting object type from those calls.

Let's see an example. Suppose you have a function called `run` that takes an object of thunks (functions in the form `() => A`) as input:

```js flow-check
function run<O: {[key: string]: (...$ReadOnlyArray<mixed>) => mixed}>(o: O): $FlowFixMe {
  return Object.keys(o).reduce<{[string]: (...$ReadOnlyArray<mixed>) => mixed}>(
    (acc, k) => ({...acc, [(k: string)]: o[k]()}),
    {},
  );
}
```

The function's purpose is to run all the thunks and return an object made of values. What's the return type of this function?

The keys are the same, but the values have a different type, namely the return type of each function.
At a value level (the implementation of the function) we're essentially mapping over the object to produce new values for the keys.
How to express this at a type level?

This is where `ObjMap<T, F>` comes in handy

```js flow-check
// let's write a function type that takes a `() => V` and returns a `V` (its return type)
type ExtractReturnType = <V>(() => V) => V;

declare function run<O: {[key: string]: (...$ReadOnlyArray<mixed>) => mixed}>(o: O): $ObjMap<O, ExtractReturnType>;

const o = {
  a: () => true,
  b: () => 'foo'
};

(run(o).a: boolean); // Works
(run(o).b: string);  // Works
(run(o).b: boolean); // Error! `b` is a string
run(o).c;            // Error! `c` was not in the original object
```

This is extremely useful for expressing the return type of functions that manipulate objects values.
You could use a similar approach (for instance) to provide the return type of bluebird's [`Promise.props`](http://bluebirdjs.com/docs/api/promise.props.html) function,
which is like `Promise.all` but takes an object as input.

Here's a possible declaration of this function, which is very similar to our first example:

```js flow-check
declare function props<A, O: {[key: string]: A}>(promises: O): Promise<$ObjMap<O, typeof $await>>;

const promises = {a: Promise.resolve(42)};
props(promises).then(o => {
  (o.a: 42); // Works
  (o.a: 43); // Error! Flow knows it's 42
});
```

## `$ObjMapi<T, F>` {#toc-objmapi}

`ObjMapi<T, F>` is similar to [`ObjMap<T, F>`](#toc-objmap). The difference is that function
type `F` will be [called](#toc-call) with both the key and value types of the elements of
the object type `T`, instead of just the value types. For example:

```js flow-check
const o = {
  a: () => true,
  b: () => 'foo'
};

type ExtractReturnObjectType = <K, V>(K, () => V) => { k: K, v: V };

declare function run<O: {...}>(o: O): $ObjMapi<O, ExtractReturnObjectType>;

(run(o).a: {k: 'a', v: boolean}); // Works
(run(o).b: {k: 'b', v: string});  // Works
(run(o).a: {k: 'b', v: boolean}); // Error! `a.k` is "a"
(run(o).b: {k: 'b', v: number});  // Error! `b.v` is a string
run(o).c;                         // Error! `c` was not in the original object
```

## `$ObjMapConst<O, T>` {#toc-objmapconst}

`$ObjMapConst<Obj, T>` is a special case of `$ObjMap<Obj, F>`, when `F` is a constant
function type, e.g. `() => T`. Instead of writing `$ObjMap<Obj, () => T>`, you
can write `$ObjMapConst<Obj, T>`. For example:
```js
const obj = {
  a: true,
  b: 'foo'
};

declare function run<O: {...}>(o: O): $ObjMapConst<O, number>;

// newObj is of type {a: number, b: number}
const newObj = run(obj);

(newObj.a: number); // Works
(newObj.b: string); // Error! Property `b` is a number
```

Tip: Prefer using `$ObjMapConst` instead of `$ObjMap` (if possible) to fix certain
kinds of `[invalid-exported-annotation]` errors.

## `$KeyMirror<O>` {#toc-keymirror}

`$KeyMirror<Obj>` is a special case of `$ObjMapi<Obj, F>`, when `F` is the identity
function type, ie. `<K>(K) => K`. In other words, it maps each property of an object
to the type of the property key. Instead of writing `$ObjMapi<Obj, <K>(K) => K>`,
you can write `$KeyMirror<Obj>`. For example:
```js
const obj = {
  a: true,
  b: 'foo'
};

declare function run<O: {...}>(o: O): $KeyMirror<O>;

// newObj is of type {a: 'a', b: 'b'}
const newObj = run(obj);

(newObj.a: 'a'); // Works
(newObj.b: 'a'); // Error! String 'b' is incompatible with 'a'
```

Tip: Prefer using `$KeyMirror` instead of `$ObjMapi` (if possible) to fix certain
kinds of `[invalid-exported-annotation]` errors.

## `$TupleMap<T, F>` {#toc-tuplemap}

`$TupleMap<T, F>` takes an iterable type `T` (e.g.: [`Tuple`](../tuples) or [`Array`](../arrays)), and a [function type](../functions) `F`,
and returns the iterable type obtained by mapping the type of each value in the iterable with the provided function type `F`.
This is analogous to the JavaScript function `map`.

Following our example from [`$ObjMap<T>`](#toc-objmap), let's assume that `run` takes an array of functions, instead of an object, and maps over them returning an array of the function call results. We could annotate its return type like this:

```js flow-check
// Function type that takes a `() => V` and returns a `V` (its return type)
type ExtractReturnType = <V>(() => V) => V

function run<A, I: Array<() => A>>(iter: I): $TupleMap<I, ExtractReturnType> {
  return iter.map(fn => fn());
}

const arr = [() => 'foo', () => 'bar'];
(run(arr)[0]: string); // Works
(run(arr)[1]: string); // Works
(run(arr)[1]: boolean); // Error!
```

## `$Call<F, T...>` {#toc-call}

`$Call<F, T...>` is a type that represents the result of calling the given [function type](../functions) `F` with 0 or more arguments `T...`.
This is analogous to calling a function at runtime (or more specifically, it's analogous to calling [`Function.prototype.call`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/call)), but at the type level; this means that function type calls happens statically, i.e. not at runtime.

Let's see a couple of examples:
```js flow-check
// Takes an object type, returns the type of its `prop` key
type ExtractPropType = <T>({prop: T, ...}) => T;
type Obj = {prop: number};
type PropType = $Call<ExtractPropType, Obj>;  // Call `ExtractPropType` with `Obj` as an argument
type Nope = $Call<ExtractPropType, {nope: number}>;  // Error! Argument doesn't match `Obj`.

(5: PropType); // Works
(true: PropType);  // Error! PropType is a number
```

```js flow-check
// Takes a function type, and returns its return type
type ExtractReturnType = <R>(() => R) => R;
type Fn = () => number;
type ReturnType = $Call<ExtractReturnType, Fn>;

(5: ReturnType);  // Works
(true: ReturnType);  // Error! ReturnType is a number
```

`$Call` can be very powerful because it allows you to make calls in type-land that you would otherwise have to do at runtime.
The type-land calls happen statically and will be erased at runtime.

```js flow-check
// Getting return types:
function getFirstValue<V>(map: Map<string, V>): ?V {
  for (const [key, value] of map.entries()) {
    return value;
  }
  return null;
}

// Using $Call, we can get the actual return type of the function above:
type Value = $Call<typeof getFirstValue, Map<string, number>>;

(5: Value);
(true: Value);  // Error! Value is a `number`

// We could generalize it further:
type GetMapValue<M> =
  $Call<typeof getFirstValue, M>;

(5: GetMapValue<Map<string, number>>);
(true: GetMapValue<Map<string, boolean>>);
(true: GetMapValue<Map<string, number>>);  // Error! value is a `number`
```

## `Class<T>` {#toc-class}

Given a type `T` representing instances of a class `C`, the type `Class<T>` is the type of the class `C`.
For example:

```js flow-check
class Store {}
class ExtendedStore extends Store {}
class Model {}

function makeStore(storeClass: Class<Store>) {
  return new storeClass();
}

(makeStore(Store): Store);
(makeStore(ExtendedStore): Store);
(makeStore(Model): Model); // Error!
```

For classes that take type parameters, you must also provide the parameter. For example:

```js flow-check
class ParamStore<T> {
  constructor(data: T) {}
}

function makeParamStore<T>(storeClass: Class<ParamStore<T>>, data: T): ParamStore<T> {
  return new storeClass(data);
}
(makeParamStore(ParamStore, 1): ParamStore<number>);
(makeParamStore(ParamStore, 1): ParamStore<boolean>); // Error!
```

## `$Exports<T>` {#toc-exports}

The following are functionally equivalent

```js
import typeof * as T from 'my-module';
```

```js
type T = $Exports<'my-module'>;
```

The advantage of the `$Exports` syntax is that you can `export` the type on the same line
```js
export type T = $Exports<'my-module'>;
```

where as you would otherwise need to export an alias in the `import typeof` case
```js
import typeof * as T from 'my-module';
export type MyModuleType = T;
```

## Deprecated utility types

### `$PropertyType<T, k>` {#toc-propertytype}

**WARNING:** `$PropertyType` is deprecated as of Flow version 0.155, and will be removed in a future version of Flow.

`$PropertyType<T, 'k'>` is equivalent to the `T['k']` [indexed access type](../indexed-access).

### `$ElementType<T, K>` {#toc-elementtype}

**WARNING:** `$ElementType` is deprecated as of Flow version 0.155, and will be removed in a future version of Flow.

`$ElementType<T, K>` is equivalent to the `T[K]` [indexed access type](../indexed-access).

### `$Partial` <UntilVersion version="0.202" />
A former alias of [Partial](#toc-partial). Support was removed in version 0.203.

### `$Shape<T>` {#toc-shape}

NOTE: **Deprecated!** This utility is unsafe - please use [`Partial`](#toc-partial) documented above to make all of an object's fields optional.

A variable of type `$Shape<T>`, where `T` is some object type, can be assigned objects `o`
that contain a subset of the properties included in `T`. For each property `p: S` of `T`,
the type of a potential binding of `p` in `o` must be compatible with `S`.

For example
```js flow-check
type Person = {
  age: number,
  name: string,
}
type PersonDetails = $Shape<Person>;

const person1: Person = {age: 28};  // ERROR: missing `name`
const person2: Person = {name: 'a'};  // ERROR: missing `age`
const person3: PersonDetails = {age: 28};  // OK
const person4: PersonDetails = {name: 'a'};  // OK
const person5: PersonDetails = {age: 28, name: 'a'};  // OK
const person6: PersonDetails = {age: '28'};  // ERROR: string is incompatible with number
```

NOTE: `$Shape<T>` is **not** equivalent to `T` with all its fields marked as optional.
In particular, Flow unsoundly allows `$Shape<T>` to be used as a `T` in several
contexts. For example in

```js
const personShape: PersonDetails = {age: 28};
(personShape: Person);
```
Flow will unsoundly allow this last cast to succeed.

It is also not equivalent to itself in some contexts:

```js flow-check
function f<T>(input: $Shape<T>): $Shape<T> {
  return input; // ERROR: `T` is incompatible with `$Shape` of `T`
}
```

This utility type is deprecated and will be deleted in the future -
use [`Partial`](#toc-partial) instead.
