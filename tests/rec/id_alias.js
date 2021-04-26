// @flow

// Excercises the reposition cache in the case of a resolved recursive type
type T = T;
declare var x : $ReadOnly<T>;
x
