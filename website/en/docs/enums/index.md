---
layout: guide
---

Flow Enums define a fixed set of constants which create their own type.

Unlike other features of Flow, Flow Enums exist as values at runtime, as well as existing as types.

[Read how to enable Flow Enums in your project](./enabling-enums/).


## Benefits <a class="toc" id="toc-benefits" href="#toc-benefits"></a>
Enums provide several benefits over existing patterns:

* Reduce repetition: Enum declarations provide both the type and the value of the enum.
* Improve Flow performance: Enums are guaranteed to have good type-checking performance,
  unlike unions which may be expensive to type-check in certain situations.
* Enable new functionality: Enums come with a `cast` [method](./using-enums/#toc-methods), which converts from a primitive type to an enum type safely.
* Enhance safety: Enums define their own type which does not implicitly coerce to and from other types (e.g. from `string`s),
  and are required to be [exhaustively checked in switch statements](./using-enums/#toc-exhaustively-checking-enums-with-a-switch). These properties can help prevent logic bugs.


## Quickstart <a class="toc" id="toc-quickstart" href="#toc-quickstart"></a>

#### [Defining enums](./defining-enums) <a class="toc" id="toc-defining-enums-defining-enums" href="#toc-defining-enums-defining-enums"></a>
An enum named `Status` with three members: `Active`, `Paused`, and `Off`.

```js
enum Status {
  Active,
  Paused,
  Off,
}
```
By default, enums define members with string values which mirror their names. You can also explicitly set values:
```js
enum Status {
  Active = 'active',
  Paused = 'paused',
  Off = 'off',
}
```
You can use numbers as well:
```js
enum Status {
  Active = 1,
  Paused = 2,
  Off = 3,
}
```
Values must be unique, literals, and all of the same type. Check out the [full docs on defining enums](./defining-enums/) to learn more.


#### [Using enums](./using-enums/) <a class="toc" id="toc-using-enums-using-enums" href="#toc-using-enums-using-enums"></a>
To access an enum member, use dot access:

```js
Status.Active
```
To use the enum type as an annotation, use the enum name:

```js
const status: Status = Status.Active;
```
Cast from the representation type (in this case, a `string`) to the enum type:

```js
const status: Status | void = Status.cast(someString);
```
You can easily provide a default value with the `??` operator:

```js
const status: Status = Status.cast(someString) ?? Status.Off;
```
Read more about the  [other methods enums provide](./using-enums/#toc-methods), including `isValid`, `members`, and `getName`.

Cast an enum type to its representation type (must be done explicitly):

```js
(status: string)
```
Checks of enums in `switch` statements are exhaustive - we ensure you check all members:
```js
// ERROR: Incomplete exhaustive check: the member `Off` of enum `Status`
// has not been considered in check of `status`.
switch (status) {
  case Status.Active: ...; break;
  case Status.Paused: ...; break;
  // We forgot to add `case: Status.Off:` here, resulting in error above.
  // Using `default:` would also work to check all remaining members.
}
```
Read more about [exhaustively checking enums](./using-enums/#toc-exhaustively-checking-enums-with-a-switch).

Check out the [the full docs on using enums](./using-enums/) to learn more.


## When to use Flow Enums <a class="toc" id="toc-when-to-use-flow-enums" href="#toc-when-to-use-flow-enums"></a>
If you previously defined a union type of literals, you can use an enum to define that type instead. Instead of

```js
type Status =
  | 'Active'
  | 'Paused'
  | 'Off';

const x: Status = 'Active';
```

or
```js
const Status = Object.freeze({
  Active: 'Active',
  Paused: 'Paused',
  Off: 'Off',
});
type StatusType = $Keys<typeof Status>;
const x: StatusType = Status.Active;
```

you can use:
```js
enum Status {
  Active,
  Paused,
  Off,
}
const x: Status = Status.Active;
```

See [migrating from legacy patterns](./migrating-legacy-patterns) to learn more about migrating legacy JavaScript enum patterns to Flow Enums.


## When to not use Flow Enums <a class="toc" id="toc-when-to-not-use-flow-enums" href="#toc-when-to-not-use-flow-enums"></a>
Enums are designed to cover many use cases and exhibit certain benefits. The design makes a variety of trade-offs to make this happen, and in certain situations,
these trade-offs might not be right for you. In those cases, you can continue to use existing patterns to satisfy your use cases.
[Read more about those situations](./using-enums/#toc-when-to-not-use-enums).
