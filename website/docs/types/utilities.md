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

`$Values<T>` represents the union type of all the value types of the enumerable properties in an [object type](../objects/),
or the elements of an [array](../arrays) or [tuple](../tuples) type (support for arrays and tuples in Flow version <SinceVersion version="0.240" />):

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

For arrays and tuples:
```js flow-check
type Arr = Array<string>;
type Str = $Values<Arr>; //=> string
's' as Str; // OK

type Tup = [1, 2];
type Num = $Values<Tup>; //=> 1 | 2
1 as Num; // OK
```

Note that using `$Values` on the [`typeof`](../typeof) an object or array literal will result in a type more general than you might expect:

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

`$ReadOnly<T>` is a type that represents the read-only version of a given [object type](../objects/)
or [tuple type](../tuples) `T` (support for tuples is for Flow <SinceVersion version="0.212" />).
A read-only object type is an object type whose keys are all [read-only](../objects/#read-only-object-properties).
Similarly, a read-only tuple is one where each element is [read-only](../tuples/#variance-annotations-and-read-only-tuples).

This means that the following are equivalent:
```js flow-check
type ReadOnlyObj = {
  +key: number,  // read-only field, marked by the `+` annotation
};
type ReadOnlyTuple = [+foo: number];
```
&rarr;
```js flow-check
type ReadOnlyObj = $ReadOnly<{
  key: number,
}>;
type ReadOnlyTuple = $ReadOnly<[number]>;
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

Additionally, other utility types, such as [spread](../objects/#object-type-spread), may strip any read/write annotations, so `$ReadOnly<T>` is a handy way to quickly make the object read-only again after operating on it:

```js flow-check
type Obj = {
  +key: number,
};

type MappedObj = $ReadOnly<{...Obj, foo: string}> // Still read-only
```

The `$ReadOnly` utility works on object and tuple types.
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

Since Flow <SinceVersion version="0.212" />, it also converts all of a tuple type's elements to be [optional](../tuples/#optional-tuple-elements).

Example for objects:
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

c as Person; // ERROR: `PersonDetails` is not a `Person` (unlike with `$Shape`)
```

For tuples:
```js flow-check
type AllRequired = [number, string];
[] as Partial<AllRequired>; // OK: like `[a?: number, b?: string]` now
```

A object or tuple of type `T` cannot be supplied to `Partial<T>`, due to mutability. You can resolve this by making the object [read-only](#toc-readonly):

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
it converts all of an object or interface’s optional fields to be required.

Since Flow <SinceVersion version="0.212" />, it also converts all of a tuple type's elements to be [required](../tuples/#optional-tuple-elements).

Example for objects:
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

For tuples:
```js flow-check
type AllOptional = [a?: number, b?: string];
[] as Required<AllOptional>; // ERROR: like `[a: number, b: string]` now
```

## `ReturnType<F>` <SinceVersion version="0.209" /> {#toc-return-type}

This utility type extracts the return type from a given function type.

```js flow-check
declare function f(s: string, n: number): boolean;
type Bool = ReturnType<typeof f>;
true as Bool;
1 as Bool; // Error: number is not boolean
```

## `Parameters<F>` <SinceVersion version="0.209" /> {#toc-parameters}

This utility type extracts the parameter types from a given function type into a [tuple type](../tuples/).

```js flow-check
declare function f(s: string, n: number): boolean;
type Tuple = Parameters<typeof f>; // Evaluates to [string, number]
's' as Tuple[0];
1 as Tuple[1];
false as Tuple[2]; // Error: tuple type only has two elements
```

## `Exclude<T, U>` <SinceVersion version="0.209" /> {#toc-exclude}

This utility type excludes all subtypes of `U` from `T`.

```js flow-check
type T = Exclude<1 | 2 | 3 | 4, 1 | 3>; // evaluates to 2 | 4
1 as T; // error
2 as T; // ok
3 as T; // error
4 as T; // ok
```

## `Extract<T, U>` <SinceVersion version="0.209" /> {#toc-extract}

This utility type retains only subtypes of `U` from `T`.

```js flow-check
declare class Car {}
declare class Animal {}
declare class Dog extends Animal {}
declare class Cat extends Animal {}
type T = Extract<Car | Dog | Cat, Animal>; // evaluates to Dog | Cat
new Car() as T; // error
new Dog() as T; // ok
new Cat() as T; // ok
```

## `ThisParameterType<F>` <SinceVersion version="0.209" /> {#toc-this-parameter-type}

This utility type extracts the type of the `this` parameter of a given function type.

```js flow-check
type T = ThisParameterType<(this: number, bar: string) => void>; // Evaluates to number
'1' as T; // error
2 as T; // ok
```

## `OmitThisParameter<F>` <SinceVersion version="0.209" /> {#toc-omit-this-parameter-type}

This utility type removes the `this` parameter from a given function type.

```js flow-check
type HasThisParamFun = (this: number, bar: string) => void;
type NoThisParamFun = OmitThisParameter<HasThisParamFun> // Evaluates to (bar: string) => void
declare const hasThisParam: HasThisParamFun;
declare const noThisParam: NoThisParamFun;

hasThisParam(''); // error: global object is not number
noThisParam(''); // ok: no this type requirement
```

## `Pick<O, Keys>` <SinceVersion version="0.211" /> {#toc-pick}

This utility type allows you to generate an object type using a subset of the fields from
another object type.

```js flow-check
type O = {foo: number, bar: string, baz: boolean};
type FooAndBar = Pick<O, 'foo' | 'bar'>;

declare const fooAndBar: FooAndBar;
fooAndBar.baz; // error: baz is missing
fooAndBar.foo as number; // ok
fooAndBar.bar as string; // ok
```

## `Omit<O, Keys>` <SinceVersion version="0.211" /> {#toc-omit}

This utility type allows you to generate an object type by omitting the specified fields from
another object type.
```js flow-check
type O = {foo: number, bar: string, baz: boolean};
type JustBaz= Omit<O, 'foo' | 'bar'>;

declare const justBaz: JustBaz;
justBaz.baz as boolean; // ok
justBaz.foo; // error: missing foo
justBaz.bar; // error: missing bar
```

## `Record<Keys, Type>` <SinceVersion version="0.211" /> {#toc-record}

This utility type allows you to generate an object type from a union of keys with the given
`Type` for each field.
```js flow-check
type NumberRecord = Record<'foo' | 'bar', number>;
declare const numberRecord: NumberRecord;
numberRecord.foo as number; // ok
numberRecord.bar as number; // ok
numberRecord.baz; // error
```

Note that `Record` is different than using an indexer:
```js flow-check
type NumberRecord = Record<'foo' | 'bar', number>;
type IndexedObject = {['foo' | 'bar']: number};

// Record uses explicit fields, which means they are all required
const rec: NumberRecord = {}; // error
// Indexers do not have this same requirement
const idx: IndexedObject = {}; // no error
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

## `$NonMaybeType<T>` {#toc-nonmaybe}

`$NonMaybeType<T>` converts a type `T` to a non-[maybe type](../maybe).
In other words, the values of `$NonMaybeType<T>` are the values of `T` except for `null` and `undefined`.

```js flow-check
type MaybeName = ?string;
type Name = $NonMaybeType<MaybeName>;

'Gabriel' as MaybeName; // Works
null as MaybeName; // Works
'Gabriel' as Name; // Works
null as Name; // Error! `null` can't be annotated as Name because Name is not a maybe type
```

## `$KeyMirror<O>` {#toc-keymirror}

`$KeyMirror<Obj>` is a special case of `$ObjMapi<Obj, F>`, when `F` is the identity
function type, ie. `<K>(K) => K`. In other words, it maps each property of an object
to the type of the property key. Instead of writing `$ObjMapi<Obj, <K>(K) => K>`,
you can write `$KeyMirror<Obj>`. For example:
```js flow-check
const obj = {
  a: true,
  b: 'foo'
};

declare function run<O: {...}>(o: O): $KeyMirror<O>;

// newObj is of type {a: 'a', b: 'b'}
const newObj = run(obj);

newObj.a as 'a'; // Works
newObj.b as 'a'; // Error! String 'b' is incompatible with 'a'
```

Tip: Prefer using `$KeyMirror` instead of `$ObjMapi` (if possible) to fix certain
kinds of `[invalid-exported-annotation]` errors.

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

makeStore(Store) as Store;
makeStore(ExtendedStore) as Store;
makeStore(Model) as Model; // Error!
```

For classes that take type parameters, you must also provide the parameter. For example:

```js flow-check
class ParamStore<T> {
  constructor(data: T) {}
}

function makeParamStore<T>(storeClass: Class<ParamStore<T>>, data: T): ParamStore<T> {
  return new storeClass(data);
}
makeParamStore(ParamStore, 1) as ParamStore<number>;
makeParamStore(ParamStore, 1) as ParamStore<boolean>; // Error!
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

## `StringPrefix` and `StringSuffix` <SinceVersion version="0.242" />
The `StringPrefix` and `StringSuffix` types represent strings with the specified prefix or suffix, respectively.
Their first type argument must be a string literal type, representing the prefix or suffix.

You could use `StringPrefix` to define a type that accepts strings which start with `data-`:

```js flow-check
type DataProp = StringPrefix<'data-'>;
'data-foo' as DataProp; // OK
'data-bar' as DataProp; // OK
'random string' as DataProp; // ERROR
```

You could use `StringSuffix` to define a type that accepts strings which end with `!`:

```js flow-check
type Exclaim = StringSuffix<'!'>;
'yay!' as Exclaim; // OK
'woo!' as Exclaim; // OK
'random string' as Exclaim; // ERROR
```

You can combine these with [intersection types](../intersections):

```js flow-check
type CSSVar = StringPrefix<'var(--'> & StringSuffix<')'>;
'var(--color)' as CSSVar; // OK
'random string' as CSSVar; // ERROR
```

Both utilities accept an optional second type argument, which is the type of the remainder:

```js flow-check
type Price = StringPrefix<'$', '1' | '2'>;
'$1' as Price; // OK
'$2' as Price; // OK
'$999' as Price; // ERROR
```

When not specified, the type of the remainder is just `string`.

## Removed utility types

These utility types used to exist, but no longer exist in latest versions of Flow.

### `$Diff<A, B>` <UntilVersion version="0.267" /> {#toc-diff}

NOTE: Please use `Omit` type instead.

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

```js
type Props = {name: string, age: number};
type DefaultProps = {age: number, other: string};
type RequiredProps = $Diff<Props, DefaultProps>; // Error!

function setProps(props: RequiredProps) {
  props.name;
  // ...
}
```

As a workaround, you can specify the property not present in `A` as optional. For example:

```js
type A = $Diff<{}, {nope: number}>; // Error!
type B = $Diff<{}, {nope: number | void}>; // Works

const a: A = {};
const b: B = {};
```

### `$Rest<A, B>` <UntilVersion version="0.266" /> {#toc-rest}

NOTE: Please use `Omit` type instead.

`$Rest<A, B>` is the type that represents the runtime object rest operation, e.g.: `const {foo, ...rest} = obj`, where `A` and `B` are both [object types](../objects/).
The resulting type from this operation will be an object type containing `A`'s *own* properties that are not *own* properties in `B`.
In flow, we treat all properties on [exact object types](../objects/#exact-and-inexact-object-types) as [own](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwnProperty).
For inexact objects, a property may or may not be own.

For example:

```js
type Props = {name: string, age: number};

const props: Props = {name: 'Jon', age: 42};
const {age, ...otherProps} = props;
otherProps as $Rest<Props, {age: number}>;
otherProps.age;  // Error!
```

The main difference with [`$Diff<A, B>`](#toc-diff), is that `$Rest<A, B>` aims to represent the true runtime rest operation,
which implies that exact object types are treated differently in `$Rest<A, B>`.
For example, `$Rest<{n: number}, {...}>` will result in `{n?: number}` because an in-exact empty object may have an `n` property,
while `$Diff<{n: number}, {...}>` will result in `{n: number}`.

### `$PropertyType<T, k>` <UntilVersion version="0.265" /> {#toc-propertytype}

`$PropertyType<T, 'k'>` is equivalent to the `T['k']` [indexed access type](../indexed-access).

### `$ElementType<T, K>` <UntilVersion version="0.265" /> {#toc-elementtype}

`$ElementType<T, K>` is equivalent to the `T[K]` [indexed access type](../indexed-access).

### `$TupleMap<T, F>` <UntilVersion version="0.247" /> {#toc-tuplemap}

`$TupleMap<T, F>` takes an iterable type `T` (e.g.: [`Tuple`](../tuples) or [`Array`](../arrays)), and a [function type](../functions) `F`,
and returns the iterable type obtained by mapping the type of each value in the iterable with the provided function type `F`.
This is analogous to the JavaScript function `map`.

Following our example from [`$ObjMap<T>`](#toc-objmap), let's assume that `run` takes an array of functions, instead of an object, and maps over them returning an array of the function call results. We could annotate its return type like this:

```js
// Function type that takes a `() => V` and returns a `V` (its return type)
type ExtractReturnType = <V>(() => V) => V

function run<A, I: Array<() => A>>(iter: I): $TupleMap<I, ExtractReturnType> {
  return iter.map(fn => fn());
}

const arr = [() => 'foo', () => 'bar'];
run(arr)[0] as string; // Works
run(arr)[1] as string; // Works
run(arr)[1] as boolean; // Error!
```

### `$Call<F, T...>` <UntilVersion version="0.247" /> {#toc-call}

NOTE: Please use [Conditional Types](../conditional) or [Indexed Access Types](../indexed-access) to extract types instead.

`$Call<F, T...>` is a type that represents the result of calling the given [function type](../functions) `F` with 0 or more arguments `T...`.
This is analogous to calling a function at runtime (or more specifically, it's analogous to calling [`Function.prototype.call`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/call)), but at the type level; this means that function type calls happens statically, i.e. not at runtime.

Let's see a couple of examples:
```js
// Takes an object type, returns the type of its `prop` key
type ExtractPropType = <T>({prop: T, ...}) => T;
type Obj = {prop: number};
type PropType = $Call<ExtractPropType, Obj>;  // Call `ExtractPropType` with `Obj` as an argument
type Nope = $Call<ExtractPropType, {nope: number}>;  // Error! Argument doesn't match `Obj`.

5 as PropType; // Works
true as PropType;  // Error! PropType is a number
```

```js
// Takes a function type, and returns its return type
type ExtractReturnType = <R>(() => R) => R;
type Fn = () => number;
type ReturnType = $Call<ExtractReturnType, Fn>;

5 as ReturnType;  // Works
true as ReturnType;  // Error! ReturnType is a number
```

`$Call` can be very powerful because it allows you to make calls in type-land that you would otherwise have to do at runtime.
The type-land calls happen statically and will be erased at runtime.

```js
// Getting return types:
function getFirstValue<V>(map: Map<string, V>): ?V {
  for (const [key, value] of map.entries()) {
    return value;
  }
  return null;
}

// Using $Call, we can get the actual return type of the function above:
type Value = $Call<typeof getFirstValue, Map<string, number>>;

5 as Value;
true as Value;  // Error! Value is a `number`

// We could generalize it further:
type GetMapValue<M> =
  $Call<typeof getFirstValue, M>;

5 as GetMapValue<Map<string, number>>;
true as GetMapValue<Map<string, boolean>>;
true as GetMapValue<Map<string, number>>;  // Error! value is a `number`
```

### `$ObjMap<T, F>` <UntilVersion version="0.246" /> {#toc-objmap}

NOTE: Please use [Mapped Types](../mapped-types) instead.

`ObjMap<T, F>` takes an [object type](../objects) `T`, and a [function type](../functions) `F`, and returns the object type obtained by mapping the type of each value in the object with the provided function type `F`. In other words, `$ObjMap` will [call](#toc-call) (at the type level) the given function type `F` for every property value type in `T`, and return the resulting object type from those calls.

Let's see an example. Suppose you have a function called `run` that takes an object of thunks (functions in the form `() => A`) as input:

```js
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

```js
// let's write a function type that takes a `() => V` and returns a `V` (its return type)
type ExtractReturnType = <V>(() => V) => V;

declare function run<O: {[key: string]: (...$ReadOnlyArray<mixed>) => mixed}>(o: O): $ObjMap<O, ExtractReturnType>;

const o = {
  a: () => true,
  b: () => 'foo'
};

run(o).a as boolean; // Works
run(o).b as string;  // Works
run(o).b as boolean; // Error! `b` is a string
run(o).c;            // Error! `c` was not in the original object
```

This is extremely useful for expressing the return type of functions that manipulate objects values.
You could use a similar approach (for instance) to provide the return type of bluebird's [`Promise.props`](http://bluebirdjs.com/docs/api/promise.props.html) function,
which is like `Promise.all` but takes an object as input.

Here's a possible declaration of this function, which is very similar to our first example:

```js
declare function props<A, O: {[key: string]: A}>(promises: O): Promise<$ObjMap<O, <T>(p: Promise<T> | T) => T>>>;

const promises = {a: Promise.resolve(42)};
props(promises).then(o => {
  o.a as 42; // Works
  o.a as 43; // Error! Flow knows it's 42
});
```

### `$ObjMapi<T, F>` <UntilVersion version="0.246" /> {#toc-objmapi}

NOTE: Please use [Mapped Types](../mapped-types) instead.

`ObjMapi<T, F>` is similar to [`ObjMap<T, F>`](#toc-objmap). The difference is that function
type `F` will be [called](#toc-call) with both the key and value types of the elements of
the object type `T`, instead of just the value types. For example:

```js
const o = {
  a: () => true,
  b: () => 'foo'
};

type ExtractReturnObjectType = <K, V>(K, () => V) => { k: K, v: V };

declare function run<O: {...}>(o: O): $ObjMapi<O, ExtractReturnObjectType>;

run(o).a as {k: 'a', v: boolean}; // Works
run(o).b as {k: 'b', v: string};  // Works
run(o).a as {k: 'b', v: boolean}; // Error! `a.k` is "a"
run(o).b as {k: 'b', v: number};  // Error! `b.v` is a string
run(o).c;                         // Error! `c` was not in the original object
```

### `$ObjMapConst<O, T>` <UntilVersion version="0.246" /> {#toc-objmapconst}

NOTE: Please use [Mapped Types](../mapped-types) instead.

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

newObj.a as number; // Works
newObj.b as string; // Error! Property `b` is a number
```

Tip: Prefer using `$ObjMapConst` instead of `$ObjMap` (if possible) to fix certain
kinds of `[invalid-exported-annotation]` errors.

### `$Partial` <UntilVersion version="0.202" />
A former alias of [Partial](#toc-partial). Support was removed in version 0.203.

### `$Shape<T>` <UntilVersion version="0.206" /> {#toc-shape}

NOTE: This utility is unsafe - please use [`Partial`](#toc-partial) documented above to make all of an object's fields optional.

A variable of type `$Shape<T>`, where `T` is some object type, can be assigned objects `o`
that contain a subset of the properties included in `T`. For each property `p: S` of `T`,
the type of a potential binding of `p` in `o` must be compatible with `S`.

For example
```js
type Person = {
  age: number,
  name: string,
};
// $FlowIgnore[deprecated-utility]
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
personShape as Person;
```
Flow will unsoundly allow this last cast to succeed.

It is also not equivalent to itself in some contexts:

```js
function f<T>(input: $Shape<T>): $Shape<T> {
  return input; // ERROR: `T` is incompatible with `$Shape` of `T`
}
```

This utility type is deprecated and will be deleted in the future -
use [`Partial`](#toc-partial) instead.
