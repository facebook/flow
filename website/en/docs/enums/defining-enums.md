---
layout: guide
---

Learn how to define a Flow Enum. Looking for a quick overview? Check out the [Quickstart Guide](../#toc-quickstart).

An enum declaration is a statement. Its name defines both a value (from which to [access its members](../using-enums/#toc-accessing-enum-members),
and call its [methods](../using-enums/#toc-methods)), and a type (which can be [used as an annotation](../using-enums/#toc-using-as-a-type-annotation) for the type of its members).

Enum members must all be of the same type, and those members can be one of four types:
[string](#toc-string-enums), [number](#toc-number-enums), [boolean](#toc-boolean-enums), and [symbol](#toc-symbol-enums).

Every enum has some common properties:

#### Consistent member type <a class="toc" id="toc-consistent-member-type" href="#toc-consistent-member-type"></a>
The type of the enum members must be consistent. For example, you can’t mix `string` and `number` members in one enum.
They must all be strings, numbers, or booleans (you do not provide values for `symbol` based enums).

#### Member name starting character <a class="toc" id="toc-member-name-starting-character" href="#toc-member-name-starting-character"></a>
Member names must be valid identifiers (e.g. not start with numbers), and must not start with lowercase `a` through `z`.
Names starting with those letters are reserved for enum [methods](../using-enums/#toc-methods) (e.g. `Status.cast(...)`).

This is not allowed:

```js
// @flow
enum Status {
  active, // Error: names can't start with lowercase 'a' through 'z'
}
```

#### Unique member names <a class="toc" id="toc-unique-member-names" href="#toc-unique-member-names"></a>
Member names must be unique. This is not allowed:

```js
// @flow
enum Status {
  Active,
  Active, // Error: the name 'Active` was already used above
}
```

#### Literal member values <a class="toc" id="toc-literal-member-values" href="#toc-literal-member-values"></a>
If you specify a value for an enum member, it must be a literal (string, number, or boolean), not a computed value. This is not allowed:

```js
// @flow
enum Status {
  Active = 1 + 2, // Error: the value must be a literal
}
```

#### Unique member values <a class="toc" id="toc-unique-member-values" href="#toc-unique-member-values"></a>
Member values must be unique. This is not allowed:

```js
enum Status {
  Active = 1,
  Paused = 1, // Error: the value has already been used above
}
```

#### Fixed at declaration <a class="toc" id="toc-fixed-at-declaration" href="#toc-fixed-at-declaration"></a>
An enum is not extendable, so you can’t add new members after the fact while your code is running.
At runtime, enum member values can’t change and the members can’t be deleted. In this way they act like a frozen object.


## String enums <a class="toc" id="toc-string-enums" href="#toc-string-enums"></a>
String enums are the default. If you don’t specify an `of` clause (e.g. `enum Status of number {}`, `enum Status of symbol {}`, etc.),
and do not specify any values (e.g. `enum Status {Active = 1}`) then the definition will default to be a string enum.

Unlike the other types of enums (e.g. number enums), you can either specify values for the enum members, or not specify values and allow them to be defaulted.

If you don’t specify values for your enum members, they default to strings which are the same as the name of your members.

```js
enum Status {
  Active,
  Paused,
  Off,
}
```
Is the same as:

```js
enum Status {
  Active = 'Active',
  Paused = 'Paused',
  Off = 'Off',
}
```
You must consistently either specify the value for all members, or none of the members. This is not allowed:

```js
// @flow
enum Status {
  Active = 'active',
  Paused = 'paused',
  Off, // Error: you must specify a value for all members (or none of the members)
}
```
Optionally, you can use an `of` clause:

```js
enum Status of string {
  Active,
  Paused,
  Off,
}
```

We infer the type of the enum based on its values if there is no `of` clause.
Using an `of` clause will ensure that if you use incorrect values, the error message will always interpret it as an enum of that type.


## Number enums <a class="toc" id="toc-number-enums" href="#toc-number-enums"></a>
Number enums must have their values specified.

You can specify a number enum like this:

```js
enum Status {
  Active = 1,
  Paused = 2,
  Off = 3,
}
```
Optionally, you can use an `of` clause:

```js
enum Status of number {
  Active = 1,
  Paused = 2,
  Off = 3,
}
```

We do not allow defaulting of number enums (unlike some other languages), because if a member from the middle of such an enum is added or removed,
all subsequent member values would be changed. This can be unsafe (e.g. push safety, serialization, logging).
 Requiring the user to be explicit about the renumbering makes them think about the consequences of doing so.

The value provided must be a number literal. (Note: there is no literal for negative numbers in JavaScript, they are the application of a unary `-` operation on a number literal.)
We could expand allowed values in the future to include certain non-literals, if requests to do so arise.


## Boolean enums <a class="toc" id="toc-boolean-enums" href="#toc-boolean-enums"></a>
Boolean enums must have their values specified. Boolean enums can only have two members.

You can specify a boolean enum like this:

```js
enum Status {
  Active = true,
  Off = false,
}
```
Optionally, you can use an `of` clause:

```js
enum Status of boolean {
  Active = true,
  Off = false,
}
```


## Symbol enums <a class="toc" id="toc-symbol-enums" href="#toc-symbol-enums"></a>
Symbol enums can’t have their values specified. Each member is a new symbol, with the symbol description set to the name of the member.
You must use the `of` clause with symbol enums, to distinguish them from string enums, which are the default when omitting values.

You can specify a symbol enum like this:

```js
enum Status of symbol {
  Active,
  Paused,
  Off,
}
```


## Flow Enums with Unknown Members <a class="toc" id="toc-flow-enums-with-unknown-members" href="#toc-flow-enums-with-unknown-members"></a>
You can specify that your enum contains "unknown members" by adding a `...` to the end of the declaration:

```js
enum Status {
  Active,
  Paused,
  Off,
  ...
}
```

When this is used, Flow will always require a `default` when [switching over the enum](../using-enums/#toc-exhaustive-checking-with-unknown-members),
even if all known enum members are checked. The `default` checks for "unknown" members you haven't explicitly listed.

This feature is useful when an enum value crosses some boundary and the enum declaration on each side may have different memebers.
For example, an enum definition which is used on both the client and the server: an enum member could be added, which would be immediately seen by the server,
but could be sent to an outdated client which isn't yet aware of the new member.

One use case for this would be the JS output of [GraphQL Enums](https://graphql.org/learn/schema/#enumeration-types):
Flow Enums with unknown members could be used instead of the added `'%future added value'` member.


## Enums at runtime <a class="toc" id="toc-enums-at-runtime" href="#toc-enums-at-runtime"></a>
Enums exist as values at runtime. We use a [Babel transform](https://www.npmjs.com/package/babel-plugin-transform-flow-enums) to transform
Flow Enum declarations into calls to the [enums runtime](https://www.npmjs.com/package/flow-enums-runtime) (read more in the [enabling enums documentation](../enabling-enums/)).
We use a runtime so all enums can share an implementation of the enum [methods](../using-enums/#toc-methods).

We use `Object.create(null)` for enums' prototype (which has the enum methods), so properties in `Object.prototype` will not pollute enums.
The only own properties of the enum object are the enum members. The members are non-enumerable (use the [`.members()` method](../using-enums/#toc-members) for that).
The entire enum object is frozen, so it cannot be modified.


## Style guide <a class="toc" id="toc-style-guide" href="#toc-style-guide"></a>

#### Naming enums <a class="toc" id="toc-naming-enums" href="#toc-naming-enums"></a>
We encourage you to define enum names in `PascalCase`, following the naming conventions of other types. All caps names (e.g. `STATUS`) are harder to read and discouraged.

We encourage you to name enums in the singular. E.g. `Status`, not `Statuses`. Just like the type of `true` and `false` is `boolean`, not `booleans`.

Don't append `Enum` to the name (e.g. don't name your enum `StatusEnum`). This is unnecessary, just like we don't append `Class` to every class name, and `Type` to every type alias.

#### Naming enum members <a class="toc" id="toc-naming-enum-members" href="#toc-naming-enum-members"></a>
We encourage you to define enum member names in `PascalCase`. All caps names (e.g. `ACTIVE`) are harder to read and discouraged.
Additionally, since Flow enforces that these are constants, you don't need to use the name to signal that intent to the programmer.

See also: the rule about [enum member name starting characters](#toc-member-name-starting-character).

#### Don't create a separate type <a class="toc" id="toc-don-t-create-a-separate-type" href="#toc-don-t-create-a-separate-type"></a>
A Flow Enum, like a class, is both a type and a value. You don't need to create a separate type alias, you can use the enum name.

#### Use dot access for accessing members <a class="toc" id="toc-use-dot-access-for-accessing-members" href="#toc-use-dot-access-for-accessing-members"></a>
Prefer `Status.Active` vs. `const {Active} = Status; Active`. This makes it easier find uses of the enum with text search, and makes it clearer to the reader what enum is involved.
 Additionally, this is required for [switch statements involving enums](../using-enums/#toc-exhaustively-checking-enums-with-a-switch).
