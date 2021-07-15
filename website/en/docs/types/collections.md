---
layout: guide
---

Flow provides two types of collections:

- A _read-only_ interface that provides operations for accessing collection elements.
- A _mutable_ interface that extends the corresponding read-only interface with write operations: adding and removing its elements.

The following figure shows all collections types:

<img style="width:100%;" alt="Collections hierarchy" src="{{ site.baseurl }}/static/collections.svg" />

Legend:

<img alt="Collections hierarchy legend" src="{{ site.baseurl }}/static/collections-legend.svg" />

## `String` <a class="toc" id="toc-string" href="#toc-string"></a>

See [Primitive Types](../primitives/#toc-strings) section

## `$ReadOnlyArray<T>` <a class="toc" id="toc-readonlyarray" href="#toc-readonlyarray"></a>

See [Array Types](../arrays/#toc-readonlyarray) section

## `Array<T>` <a class="toc" id="toc-array" href="#toc-array"></a>

See [Array Types](../arrays/#toc-array-type) section

## `$ReadOnlySet<T>` <a class="toc" id="toc-readonlyset" href="#toc-readonlyset"></a>

`$ReadOnlySet` stores unique elements.

For example:

```js
const numbersSet: $ReadOnlySet<string, number> = new Set([0, 1, 2, 3]);
const hasZero = numbersSet.has(0); // ok
numbersSet.delete(1); // error, read-only set
numbersSet.add(3); // error, read-only set
```

## `Set<T>` <a class="toc" id="toc-set" href="#toc-set"></a>

[Set](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) collection in MDN.

Subtype of `$ReadOnlySet<T>` with write operations:

```js
const mySet = new Set(); // Set<number | string>

mySet.add(1);
mySet.add(5);
mySet.add(5);
mySet.add("some text");
mySet.delete(1);
```

## `$ReadOnlyMap<K, V>` <a class="toc" id="toc-readonlymap" href="#toc-readonlymap"></a>

`$ReadOnlyMap` stores key-value pairs (or entries).

For example:

```js
const numbersMap: $ReadOnlyMap<string, number> = new Map([
  ["key1", 0],
  ["key2", 1]
]);
const firstNumber = numbersMap.get("key1"); // ok
numbersMap.delete("key1"); // error, read-only map
numbersMap.set("key1", 3); // error, read-only map
```

## `Map<K, V>` <a class="toc" id="toc-map" href="#toc-map"></a>

[Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) collection in MDN.

Subtype of `$ReadOnlyMap<K, V>` with write operations:

```js
const sayings = new Map(); // Map<string, string>
sayings.set("dog", "woof");
sayings.set("cat", "meow");
sayings.set("elephant", "toot");
```

## `$ReadOnlyWeakMap<K, V>` <a class="toc" id="toc-readonlyweakmap" href="#toc-readonlyweakmap"></a>

Where `K` should be subtype of object or `$ReadOnlyArray`.

`$ReadOnlyWeakMap` is a collection of key/value pairs in which the keys are weakly referenced.

```js
const key0 = { a: "foo" };
const numbersMap: $ReadOnlyWeakMap<{| a: string |}, number> = new WeakMap([
  [key0, 1]
]);
const firstNumber = numbersMap.get(key0); // ok
numbersMap.delete(key0); // error, read-only weakmap
numbersMap.set(key0, 3); // error, read-only weakmap
```

## `WeakMap<K, V>` <a class="toc" id="toc-weakmap" href="#toc-weakmap"></a>

[WeakMap](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap) collection in MDN.

Subtype of `$ReadOnlyWeakMap<K, V>` with write operations:

```js
```

## `$ReadOnlyWeakSet<T>` <a class="toc" id="toc-readonlyweakset" href="#toc-readonlyweakset"></a>

Where `T` should be subtype of object or `$ReadOnlyArray`.

`$ReadOnlyWeakSet` lets you store weakly held objects in a collection.

```js
```

## `WeakSet<T>` <a class="toc" id="toc-weakset" href="#toc-weakset"></a>

[WeakSet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakSet) collection in MDN.

Subtype of `$ReadOnlyWeakSet<T>` with write operations:

```js
```
