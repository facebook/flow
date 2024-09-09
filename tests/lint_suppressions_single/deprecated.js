type Foo = $Call<() => string>; // error
// $FlowFixMe[deprecated-type]
type Bar = $Call<() => string>; // suppressed
