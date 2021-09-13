---
layout: guide
---

Learn how to migrate to Flow Enums from legacy JavaScript enum patterns like `Object.freeze`.

First, learn how to [update the enum definition site](#toc-updating-definitions), and next learn how to [update files that import and use the enum](#toc-updating-usage).

## Updating definitions <a class="toc" id="toc-updating-definitions" href="#toc-updating-definitions"></a>

#### Object.freeze <a class="toc" id="toc-object-freeze" href="#toc-object-freeze"></a>
If you are using `Object.freeze`, you can migrate to an enum if the values of the object are:

* All the same primitive type, and that type is `boolean`, `string`, `number`, or `symbol`.
* All literals.
* Contain no duplicate values.

Replace

```js
const Status = Object.freeze({
  Active: 1,
  Paused: 2,
  Off: 3,
});

export type StatusType = $Values<typeof Status>;

export default Status;
```

with
```js
export default enum Status {
  Active = 1,
  Paused = 2,
  Off = 3,
}
```

- Check to ensure that the key names do not start with lowercase ‘a’-‘z’ (disallowed in enums). If they do, you’ll need to rename the member names.
- Remove any usage of `$Keys<...>` or `$Values<...>` on the enum type, these are no longer needed as a Flow Enum defines a type itself (its name).
- Delete any type exports based on the enum, as you just need to export the Flow Enum. A Flow Enum acts as both a type and a value (like a class).

Then, take a look at [how to update files that import and use the enum](#toc-updating-usage).


#### keyMirror <a class="toc" id="toc-keymirror" href="#toc-keymirror"></a>
The `keyMirror` utility creates an object whose values are mirrors of its key names. You can replace `keyMirror` usage with a string based enum.

Replace

```js
import keyMirror from 'keyMirror';

const Status = keyMirror({
  Active: null,
  Paused: null,
  Off: null,
});

export type StatusType = $Keys<typeof Status>;

export default Status;
```

with

```js
export default enum Status {
  Active,
  Paused,
  Off,
}
```

- Check to ensure that the key names do not start with lowercase ‘a’-‘z’ (disallowed in enums). If they do, you’ll need to rename the member names.
- Remove any usage of `$Keys<...>` on the enum type, it's no longer needed as a Flow Enum defines a type itself (its name).
- Delete any type exports based on the enum, you just need to export the Flow Enum. A Flow Enum acts as both a type and a value (like a class).

Then, take a look at [how to update files that import and use the enum](#toc-updating-usage).


## Updating usage <a class="toc" id="toc-updating-usage" href="#toc-updating-usage"></a>

#### Fix type imports <a class="toc" id="toc-fix-type-imports" href="#toc-fix-type-imports"></a>
Previous patterns required you to export (and then import) a type separate from the enum itself.
Flow Enums are both types and values (like a class), so you just need to export the Flow Enum itself. Since there is now one export, you only need one import.
Read more about [exporting enums](../using-enums/#toc-exporting-enums) and [importing enums](../using-enums/#toc-importing-enums).

If you previously had:
```js
const Status = Object.freeze({
  Active: 1,
  Paused: 2,
  Off: 3,
});
export type StatusType = $Values<typeof Status>;
export default Status;
```

And you've replaced it with:
```js
export default enum Status {
  Active = 1,
  Paused = 2,
  Off = 3,
}
```

Then you need to fix the imports as well:

##### If both type and value were imported <a class="toc" id="toc-if-both-type-and-value-were-imported" href="#toc-if-both-type-and-value-were-imported"></a>
For a user of the enum, if you previously imported both the type and the value, you can delete the type import and update annotations used.

Change
```js
import type {StatusType} from 'status';

import Status from 'status';

const myStatus: StatusType = Status.Active;
```
to
```js
// Type import is deleted

import Status from 'status';

const myStatus: Status = Status.Active; // Changed type annotation to just `Status`
```

##### If only the type was imported <a class="toc" id="toc-if-only-the-type-was-imported" href="#toc-if-only-the-type-was-imported"></a>
For a user of the enum, if you previously imported just the type, change the type import to a default import rather than a named import.

Change
```js
import type {StatusType} from 'status';

function isActive(status: StatusType) { ... }
```
to

```js
// Remove the braces `{` `}` and changed the name - this is a default import now
import type Status from 'status';

function isActive(status: Status) { ... } // Changed type annotation to just `Status`
```


#### Mapping enums to other values <a class="toc" id="toc-mapping-enums-to-other-values" href="#toc-mapping-enums-to-other-values"></a>
Sometimes you want to map from an enum value to some other value. Previously, we sometimes used object literals for this.
With Flow Enums, use a function with a `switch` instead. The switch is [exhaustively checked](../using-enums/#toc-exhaustively-checking-enums-with-a-switch),
so Flow will ensure you update your mapping when you add or remove Flow Enum members.

Replace this pattern

```js
const STATUS_ICON: {[Status]: string} = {
  [Status.Active]: 'green-checkmark',
  [Status.Paused]: 'grey-pause',
  [Status.Off]: 'red-x',
};
const icon = STATUS_ICON[status];
```

with

```js
function statusIcon(status: Status): string {
  switch (status) {
    case Status.Active:
      return 'green-checkmark';
    case Status.Paused:
      return 'grey-pause';
    case Status.Off:
      return 'red-x';
  }
}
const icon = statusIcon(status);
```
Read more about [mapping enums to other values](../using-enums/#toc-mapping-enums-to-other-values).


#### Usage as the representation type (e.g. a string) <a class="toc" id="toc-usage-as-the-representation-type-e-g-a-string" href="#toc-usage-as-the-representation-type-e-g-a-string"></a>
You can't use a Flow Enum directly as its representation type (e.g. a `string`).
If you get Flow errors about using an enum as its representation type, first try to refactor your code so that it expects the enum type instead of the representation type
(e.g. change annotations from `string` to `Status`). If you really want to use the enum as its representation type, you can add in explicit casts.
See [casting to represetation type](../using-enums/#toc-casting-to-representation-type).


#### Casting to the enum type <a class="toc" id="toc-casting-to-the-enum-type" href="#toc-casting-to-the-enum-type"></a>
If before you cast from an enum's representation type (e.g. `string`) to the enum type with something like this:

```js
function castToStatus(input: number): StatusType | void {
  switch(input) {
    case 1: return Status.Active;
    case 2: return Status.Paused;
    case 3: return Status.Off;
    default: return undefined;
  }
}

castToStatus(x);
```

You can now just use the [cast](../using-enums/#toc-cast) method:

```js
Status.cast(x);
```


#### Update switch statements <a class="toc" id="toc-update-switch-statements" href="#toc-update-switch-statements"></a>
Flow Enums are exhaustively checked in `switch` statements. You might need to update your code when you are switching over an enum value.
Read more at [exhaustively checking enums in switch statements](../using-enums/#toc-exhaustively-checking-enums-with-a-switch).


#### Operations over enum members <a class="toc" id="toc-operations-over-enum-members" href="#toc-operations-over-enum-members"></a>
If previously you used functionality like `Object.values`, `Object.keys`, or `for-in` loops to get and operate on the enum members,
you can use the [members method](../using-enums/#toc-members) instead.
