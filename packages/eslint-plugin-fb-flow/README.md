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
