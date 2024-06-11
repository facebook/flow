function test_NonMaybe() {
  42 as number | null as $NonMaybeType<mixed>; // error null ~> MixedT (NonMaybe)

  42 as number | void as $NonMaybeType<mixed>; // error undefined ~> MixedT (NonMaybe)

  42 as ?number as $NonMaybeType<mixed>; // error null | undefined ~> MixedT (NonMaybe)
}

function test_function(fn: mixed) {
  if (typeof fn === "function") {
      const one = 1 as typeof fn; // error number ~> MixedT (Mixed_function)
      one.apply;
  }
}

function test_truthy(x: mixed) {
  if (x) {
      const false_ = false as typeof x; // error false ~> MixedT (Mixed_truthy)
      if (!false_) { // then branch taken
          false_ as empty;
      }
  }
}

function test_function_implicit_call(
  functor: <F>(f: F) => any,
  data: mixed,
) {
  const dataArr: mixed = typeof data === 'function' ? functor(data)() : data; // okay
}

function test_optional(obj: {a?: number}) {
  obj.a as $NonMaybeType<mixed>; // error undefined ~> MixedT (non-maybe)
  obj.a as number; // error
}
