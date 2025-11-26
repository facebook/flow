// @flow

// Test 1: Empty record - should suggest all fields
record Person {
  name: string,
  age: number,
  active: boolean,
}

const p1 = Person {  };
//                  ^

// Test 2: Partial field name - should filter suggestions
const p2 = Person { na };
//                    ^

// Test 3: After one field - should suggest remaining fields
const p3 = Person { name: "John",  };
//                                ^

// Test 4: After one field with partial - should filter remaining
const p4 = Person { name: "John", a };
//                                  ^

// Test 5: All fields used - should not suggest anything
const p5 = Person { name: "John", age: 25, active: true,  };
//                                                        ^

// Test 6: Record with optional fields
record User {
  id: string,
  email: string = "",
  verified: boolean,
}

const u1 = User {  };
//                ^

// Test 7: After required field - should still suggest optional fields
const u2 = User { id: "123",  };
//                           ^

// Test 8: Nested records
record Address {
  street: string,
  city: string,
}

record Employee {
  name: string,
  address: Address,
}

const e1 = Employee {  };
//                    ^

const e2 = Employee { name: "Jane",  };
//                                  ^

// Test 9: Generic record
record Container<T> {
  content: T,
  id: string,
}

const c1 = Container {  };
//                     ^

// Test 10: Record with methods (should not suggest methods)
record WithMethods {
  field: string,
  method(): void {}
}

const wm1 = WithMethods {  };
//                        ^
