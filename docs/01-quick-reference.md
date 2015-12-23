---
id: quick-reference
title: Quick Reference
layout: docs
permalink: /docs/quick-reference.html
prev: coming-soon.html
next: type-annotations.html
---

### Built-in Primitive Types

• [**`any`**](#the-any-primitive-type) - Describes *any* possible type<br />
• [**`boolean`**](#the-boolean-primitive-type) - Describes `true` or `false` values<br />
• [**`mixed`**](#the-mixed-primitive-type) - Describes the supertype of *all* types<br />
• [**`number`**](#the-number-primitive-type) - Describes literal number values<br />
• [**`string`**](#the-string-primitive-type) - Describes literal string values<br />
• [**`void`**](#the-void-primitive-type) - Describes `undefined` values<br />

### Built-in Class Types

• [**`Array<T>`**](#the-array-t-constructor) - Describes `Array` objects with elements of type `T`<br />
• [**`Boolean`**](#the-boolean-constructor) - Describes `Boolean` objects (but not `boolean` literal values!)<br />
• [**`Class<T>`**](#the-class-t-constructor) - Describes the type of the class object that would instantiate an instance of `T`<br />
• [**`Function`**](#the-function-constructor) - Describes *any* function<br />
• [**`Number`**](#the-number-constructor) - Describes `Number` objects (but not `number` literal values!)<br />
• [**`Object`**](#the-object-constructor) - Describes *any* object<br /> 
• [**`String`**](#the-string-constructor) - Describes `String` objects (but not `string` literal values!)<br />

### Built-in Syntactic Types

• [Object Signature: **`{prop1: number, prop2: string}`**](#the-object-signature-type) - Describes the type of objects that match a specified signature <br />
• [Function Signature: **`(p1: number, p2: string) => boolean`**](#the-function-signature-type) - Describes the type of functions that match a specified signature <br />
• [Callable-Object/Function-Object Signature: **`{ (p1: number, p2: string): boolean, prop2: number }`**](#the-callable-object-function-object-type) - Describes the type of functions that may have additional "static" properties on them <br />

### Other Built-In Types

Flow comes with a [large interface definition](https://github.com/facebook/flow/blob/master/lib/core.js) that describes all of the other language built-ins for JavaScript. It is included for you by default when you run Flow.

### User Defined Constructor Types

User-defined class names and function names can be used as a named type annotation as well. This kind of annotation describes the type of the **instance** the class/function would instantiate if `new`-ed.

Example:

```javascript
class MyClass {
  myMethod() { return 42; }
}

function MyConstructorFunc() {}
MyConstructorFunc.prototype.myMethod = function() { return 42; };

const a: MyClass = new MyClass(); // Valid!
const b: number = a.myMethod(); // Valid!

const c: MyConstructorFunc = new MyConstructorFunc(); // Valid!
const d: number = c.myMethod(); // Valid!
```

## Notes And Caveats
- The `+` operator works on numbers **and** strings in JavaScript. In the case of a using `+` on a `number` and `string`, the `number` is implicitly converted to a `string` via `toString()` and a concatenation takes place. Flow recognizes and allows this behavior.
- `Date` objects implicitly convert to `number` when used in arithmetic operations.
Flow supports this behavior.
- Use `mixed` to annotate a location that can take anything, but do not use `Object` instead! It is confusing to view everything as an object, and if by any chance you do mean "any object", there is a better way to specify that, just as there is a way to specify "any function". Also, be aware of the difference between `mixed` and `any`: `mixed` is a taint that propagates quickly, since no useful operation can be performed on it.

<hr />

## • The `any` primitive type
This is a special type that represents *any* value. An `any` type may flow into any other type, and all other types may flow into an `any` type.

This type can be useful when you don't want Flow to check the type of a value. Sometimes this is desirable if you're not sure what the type should be (i.e. it comes from a 3rd party library and isn't easily described) or if Flow is giving you an error for some code that you're certain is safe -- despite the fact that it doesn't typecheck.

Using this "backdoor" is dangerous and not recommended, but it is sometimes necessary because of the gradual nature of Flow. `any` allows you to model code where Flow does not have knowledge (i.e. when you reference something in a non-Flow file or library).

Example:

```javascript
const a: any = 'some string'; // Valid!
const b: any = undefined; // Valid!
const c: any = 42; // Valid!

function foo(): any { return 42; }
const d: string = foo(); // Valid! ('any' may flow into any other type!)
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `Array<T>` constructor
This type describes JavaScript array objects and the type of the elements contained within the array. Flow assumes that arrays are not sparse and that any element access on a value of type `Array<T>` will result in a type `T`.

For arrays that have a fixed size and/or elements whose types aren't homogenous, consider using a [tuple annotation](arrays.html#tuples) for more accurate typechecking.

Example:

```javascript
const a: Array<number> = [42]; // Valid!
const b: Array<number> = ['some string']; // Type error!
const c: Array<number> = [42, 'some string']; // Type error!

const myNumbers: Array<number> = [42];
const d: number = myNumbers[0]; // Valid!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `boolean` primitive type
This type describes a boolean value in JavaScript. The possible values of this type are `true` and `false`.

Note that there are many implicit conversions to `boolean` in JavaScript. Flow understands this and allows it when it is safe to do so. For example, any expression can be used as a conditional in an if-statement or as an operand to `&&`.

Example:

```javascript
const a: boolean = true; // Valid!
const b: boolean = false; // Valid!
const c: boolean = 42; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `Boolean` constructor
This type describes objects created from JavaScript's built-in `Boolean` constructor. Note that this is distinct from literal `true` or `false` values! (although the methods of `Boolean` are also available on the primitive `boolean` type as well).

Example:

```javascript
const a: Boolean = new Boolean(true); // Valid!
const b: Boolean = true; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `Class<T>` constructor
This type describes the type of the class object that one would use to instantiate an instance of `T`.

Normally when you specify the name of a class in a type annotation, this would refer to the type of **instances** of that class. However, sometimes it's useful to describe the type of the class object iself; So this type allows you to do this.

This is similar to a [`typeof` annotation](typeof.html) except that it is limited to usage for classes rather than all values.

Example:

```javascript
class MyClass {
  myMethod() { return 42; }
}

const a: Class<MyClass> = MyClass; // Valid!
const b: MyClass = new a(); // Valid!

const c: Class<MyClass> = new MyClass(); // Type error!

const d = 42;
const e: Class<d> = Number; // Error! 'd' is a runtime value, not a type!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `Function` constructor
This type describes *any* function. Any kind of function may flow into it, and any interface that specifies any kind of function (no matter how specific) may receive it. This type works exactly like the [`any`](#the-any-primitive-type) type except that it is restricted to function types.

Example:

```javascript
const a: Function = function() {}; // Valid!
const b: Function = p => p; // Valid!
const c: Function = 42; // Type error!

// Valid! (`Function` may flow into any function type)
function foo(): Function { 
  return function(x: number): number { return x; }
}
const d: (str: string) => string = foo(); // Valid!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `mixed` primitive type
This is a special type that represents the supertype of all values. Any other type may flow into a `mixed` type, but `mixed` may only flow into other `mixed` types.

Beware of the difference between `mixed` and `any`; `mixed` is a taint that ensures the type doesn't flow into a well-typed interface but allows any other type to flow into it. If you wish to flow a `mixed` variable into a well-typed interface, you must first [refine](dynamic-type-tests.html) the type before using it.

Example:

```javascript
const a: mixed = 'some string'; // Valid!
const b: mixed = undefined; // Valid!
const c: mixed = 42; // Valid!

function foo(): mixed { return 42; }
const d: string = foo(); // Error! ('mixed' may not flow to any other type!)
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `number` primitive type
This type describes a number in JavaScript.

Example:

```javascript
const a: number = 42; // Valid!
const b: number = 'str'; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `Number` constructor
This type describes objects created from JavaScript's built-in `Number` constructor. Note that this is distinct from number literal values! (although the methods of `Number` are also available on the primitive `number` type as well).

Example:

```javascript
const a: Number = new Number(42); // Valid!
const b: Number = 42; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `Object` constructor
This type describes *any* object. Any kind of object may flow into it, and any interface that specifies any kind of object (no matter how specific) may receive it. This type works exactly like the [`any`](#the-any-primitive-type) type except that it is restricted to object types.

Example:

```javascript
const a: Object = {}; // Valid!
const b: Object = new RegExp(); // Valid!
const d: Object = 42; // Type error!

function foo(): Object { return {}; }
const e: MyClass = foo(); // Valid! (`Object` may flow into any object type)
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `string` primitive type
This type describes a string in JavaScript.

Example:

```javascript
const a: string = 'some string'; // Valid!
const b: string = 42; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `String` constructor
This type describes objects created from JavaScript's built-in `String` constructor. Note that this is distinct from string literal values! (although the methods of `String` are also available on the primitive `string` type as well).

Example:

```javascript
const a: String = new String('hai!'); // Valid!
const b: String = 'hai!'; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The `void` primitive type
This type describes an `undefined` value in JavaScript. 

Normally you only want to use this type to describe the return type of functions that return nothing. The only values that have type `void` are `undefined` and the results of the `void()` operator in JavaScript. Flow is fairly strict about things that `void` may flow into.

Example:

```javascript
const a: void = undefined; // Valid!
const b: void = 42; // Type error!
```
<sub><a href="#built-in-primitive-types">Back To Top</a></sub>

## • The object signature type
This type describes any object values that match the specified interface.

Example:

```javascript
// Valid!
const a: {prop1: number, prop2: string} = {
  prop1: 42,
  prop2: 'asdf',
};

// Type error! Missing a `prop2` property
const b: {prop1: number, prop2: string} = {
  prop1: 42,
};

// Type error! `prop2 property types don't match!
const c: {prop1: number, prop2: string} = {
  prop1: 42,
  prop2: 42, // Type error!
};
```
<sub><a href="#built-in-syntactic-types">Back To Top</a></sub>

## • The function signature type
This type describes any function values that match the specified parameters/return signature.

Example:

```javascript
// Valid!
const add: ((num1: number, num2: number) => number) = function(num1, num2) {
  return num1 + num2;
};

// Type error! Parameter types don't match!
const badTypeAnnotation: ((num1: number, num2: number) => number) =
  function(str1: string, str2: string) {
    return str1.length + str2.length;
  };
```
<sub><a href="#built-in-syntactic-types">Back To Top</a></sub>

## • The callable-object/function-object type
This type describes function values that might have additional static properties set on them.

Example:

```javascript
// Valid!
function getStrProcessor(token: string): { (str: string): string, token: string} {
  const processor = function(str) {
    return str.replace(token, '<<PROCESSED>>');
  };
  processor.token = token;
  return processor;
}

// Type error! Note that the returned function is missing a `token` property
function getStrProcessor(token: string): { (str: string): string, token: string} {
  return function(str) {
    return str.replace(token, '<<PROCESSED>>');
  };
}
```
<sub><a href="#built-in-syntactic-types">Back To Top</a></sub>
