function guard(value: unknown): value is string {
  return true; // ERROR
}

function assertion(value: unknown): asserts value is string {} // ERROR

function bareAssertion(value: unknown): asserts value {} // ERROR
