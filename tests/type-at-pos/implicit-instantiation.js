//@flow

function identity<T>(x: T): T { return x }

identity<_>(3);
