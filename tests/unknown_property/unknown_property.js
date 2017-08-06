// @flow
type Typed = { foo: string, bar: ?string, baz?:?string };
let TypedObj: Typed = { foo: 'fooStr', bar: null };

if (TypedObj.foo != null) {}
if (TypedObj.bar != null) {}
if (TypedObj.baz != null) {}
if (TypedObj.bing != null) {} // Error

// sealed objects are not affected
let SealedObj = { foo: 'fooStr', bar: null };

if (SealedObj.foo != null) {}
if (SealedObj.bar != null) {}
if (SealedObj.baz != null) {}
if (SealedObj.bing != null) {}

// unsealed objects are not affected
let UnsealedObj = { foo: 'fooStr', bar: null };

if (UnsealedObj.foo != null) {}
if (UnsealedObj.bar != null) {}
if (UnsealedObj.baz != null) {}
if (UnsealedObj.bing != null) {}
