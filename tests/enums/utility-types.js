// @flow

enum E {
  A,
  B,
}

const trigger = null;

// $PropertyType
E.A as $PropertyType<typeof E, 'A'>; // OK
trigger as $PropertyType<typeof E, 'X'>; // Error: `X` is not a member of enum `E`

// $ElementType
trigger as $ElementType<typeof E, string>; // Error: computed access is not allowed on enums

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

// $ObjMap
trigger as $ObjMap<typeof E, <T>(T) => [T]>; // Error: enum `E` is not a valid arg to $ObjMap

// $ObjMapi
trigger as $ObjMapi<typeof E, <K, V>(K, V) => [K, V]>; // Error: enum `E` is not a valid arg to $ObjMapi
