# eslint-plugin-fb-flow

This is a set of ESLint rules created and published by the Flow team. They are in addition to (not a replacement for) the rules of [`eslint-plugin-flowtype`](https://github.com/gajus/eslint-plugin-flowtype) created and published by the open-source community.

## Usage

Add `fb-flow` to the plugins section of your `.eslintrc` configuration file. You can omit the `eslint-plugin-` prefix:

```json
{
    "plugins": [
        "fb-flow"
    ]
}
```

Then configure the rules you want to use under the rules section.

```json
{
    "rules": {
        "fb-flow/use-indexed-access-type": 2
    }
}
```

## Rules

### `use-indexed-access-type`
You should use Flow's [Indexed Access types](https://flow.org/en/docs/types/indexed-access/) instead of the `$PropertyType<...>` and `$ElementType<...>` utility types.

#### `$PropertyType`
Instead of
```
type T = $PropertyType<Foo, 'bar'>;
```
write
```
type T = Foo['bar'];
```

#### `$ElementType`
Instead of
```
type T = $ElementType<Foo, K>;
```
write
```
type T = Foo[K];
```

#### Autofixer
This rule includes an autofixer that can fix most cases. It does not handle cases with comments inside the type arguments to `$PropertyType` and `$ElementType`.

The autofixer does not remove nested `$NonMaybeType`s and output Optional Indexed Access Types.
In general `$ElementType<$NonMaybeType<O>, K>` is not equivalent to `O?.[K]` as Optional Indexed Access Types are modelled after optional chaining, so have a `void` in their resulting type.
An auto-fixer can't just naively wrap the whole thing with `$NonMaybeType` as the type of the property at `K` might be nullable, so doing so would remove that nullability.


### `use-exact-by-default-object-type`
For Flow projects which turn on [exact objects by default](https://flow.org/en/docs/types/objects/#toc-explicit-inexact-object-types),
this ESLint rule enforces that you use the `{ prop: type }` syntax for exact object types instead of the `{| prop: type |}` syntax.

This rules includes an autofixer that transforms `{| prop: type |}` into `{ prop: type }`.

#### Invalid
```
type Props = {|
  foo: string,
|};
```

#### Valid
```
type Props = {
  foo: string,
};
```
```
type InexactProps = {
  foo: string,
  ...
};
```


### `use-flow-enums`
You should use [Flow Enums](https://flow.org/en/docs/enums/) instead of legacy enum patterns (e.g. `keyMirror` and `Object.freeze`).

If this lint has flagged an object which is conceptually not an enum (e.g. a bag of constants that don't define a type), you can ignore this warning.

There are also [other reasons to not use Flow Enums](https://flow.org/en/docs/enums/#toc-when-to-not-use-flow-enums), and if any of those are relevant to you, ignore this warning.

See the [Migrating from legacy patterns](https://flow.org/en/docs/enums/migrating-legacy-patterns/) for how to fix this issue.

#### Examples
Examples of ***invalid*** code for this rule:

```
const Foo = Object.freeze({
  A: 1,
  B: 2,
});

const Bar = keyMirror({
  A: null,
  B: null,
});
```

Examples of ***valid*** code for this rule:
```
enum Foo {
  A = 1,
  B = 2,
};

enum Bar {
  A,
  B,
};
```


### `flow-enums-default-if-possible`
With [Flow Enums](https://flow.org/en/docs/enums/),
if you don't specify member values they by default become [strings mirrored from the member name](https://flow.org/en/docs/enums/defining-enums/#toc-string-enums).

Instead of:
```
enum Status {
  Active = 'Active',
  Paused = 'Paused',
  Off = 'Off',
}
```

Write:
```
enum Status {
  Active,
  Paused,
  Off,
}
```

This lint comes with an autofixer to automatically make the fix.


### `no-flow-enums-object-mapping`
You should use a function with a `switch` instead of an object literal to map [Flow Enums](https://flow.org/en/docs/enums/) to other values -
see the [docs](https://flow.org/en/docs/enums//using-enums/#toc-mapping-enums-to-other-values).
This avoids having to cast to `string` and [exhaustively checks the enum](https://flow.org/en/docs/enums/using-enums/#toc-exhaustively-checking-enums-with-a-switch).

If you have the Flow Enum:
```
enum Status {
  Active,
  Paused,
  Off,
}
```

Instead of:
```
const STATUS_ICON: {[Status]: string} = {
  [(Status.Active: string)]: 'green-checkmark',
  [(Status.Paused: string)]: 'grey-pause',
  [(Status.Off: string)]: 'red-x',
};
const icon = STATUS_ICON[status];
```

Write:
```
function getStatusIcon(status: Status): string {
  switch (status) {
    case Status.Active:
      return 'green-checkmark';
    case Status.Paused:
      return 'grey-pause';
    case Status.Off:
      return 'red-x';
  }
}
const icon = getStatusIcon(status);
```

If you add a new member to `Status`, Flow will error and tell you to update your `switch` statement.
