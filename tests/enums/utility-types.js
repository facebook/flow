// @flow

enum E {
  A,
  B,
}

const trigger = null;

// Indexed Access Types
E.A as (typeof E)['A']
trigger as (typeof E)['X']; // Error: `X` is not a member of enum `E`
trigger as (typeof E)[string]; // Error: computed access is not allowed on enums

// $Diff
trigger as $Diff<typeof E, {A: E}>; // Error: enum  `E` is not an object

// $ReadOnly
trigger as $ReadOnly<typeof E>; // Error: enum `E` is not an object

// $Keys
'A' as $Keys<typeof E>; // Error: TODO: improve error

// $Values
trigger as $Values<typeof E>; // Error

// $Exact
E as $Exact<typeof E>; // Error: TODO: improve error

// $Rest
trigger as $Rest<typeof E, {A: E}>; // Error: enum  `E` is not an object

// mapped type
trigger as {[K in keyof typeof E]: E[K]}; // Error: enum `E` is not an object
