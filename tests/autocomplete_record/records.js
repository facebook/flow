// @flow

// Test 1: Basic record with primitive types
record Person {
  name: string,
  age: number,
  active: boolean,
}

const p = Pe
//          ^

// Test 2: Record with optional fields (should only suggest required fields)
record User {
  id: string,
  email: string = "",
  verified: boolean,
}

const u = Us
//          ^

// Test 3: Record with complex types
type ComplexType = mixed;

record Data {
  value: ComplexType,
  ref: mixed,
}

const d = Da
//          ^

// Test 4: Generic record
record Container<T> {
  content: T,
  id: string,
}

const c = Con
//           ^

// Test 5: Record with all optional fields (should not generate snippet)
record Optional {
  a: number = 1,
  b: string = "",
}

const opt = Opt
//             ^

// Test 6: Record with methods (should exclude methods from completion)
record WithMethods {
  field: string,
  method(): void {}
}

const wm = WithMe
//               ^


// Test 7: Record with statics (should exclude methods from completion)
record WithStatics {
  field: string,
  static bar: number = 3
}

const ws = WithSt
//               ^
