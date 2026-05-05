function test_NonMaybe() {
  42 as number | null as NonNullable<unknown>; // error null ~> MixedT (NonMaybe)

  42 as number | void as NonNullable<unknown>; // error undefined ~> MixedT (NonMaybe)

  42 as ?number as NonNullable<unknown>; // error null | undefined ~> MixedT (NonMaybe)
}

function test_function(fn: unknown) {
  if (typeof fn === "function") {
      const one = 1 as typeof fn; // error number ~> MixedT (Mixed_function)
      one.apply;
  }
}

function test_truthy(x: unknown) {
  if (x) {
      const false_ = false as typeof x; // error false ~> MixedT (Mixed_truthy)
      if (!false_) { // then branch taken
          false_ as empty;
      }
  }
}

function test_function_implicit_call(
  functor: <F>(f: F) => any,
  data: unknown,
) {
  const dataArr: unknown = typeof data === 'function' ? functor(data)() : data; // okay
}

function test_optional(obj: {a?: number, ...}) {
  obj.a as NonNullable<unknown>; // error undefined ~> MixedT (non-maybe)
  obj.a as number; // error
}
