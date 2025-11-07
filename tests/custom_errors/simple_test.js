// @flow

// Test 1: Normal type alias without custom error
type NormalString = string;

const normalTest1: NormalString = 42; // Error: number is not compatible with string
const normalTest2: NormalString = "hello"; // OK

// Test 2: Type alias with @flowCustomError for primitive types
/**
 * @flowCustomError
 * @description custom desc
 */
type UserId = string;

const userTest1: UserId = 42; // Error: Should show custom error
const userTest2: UserId = "user123"; // OK

// Test 3: Type alias with @flowCustomError for object types
/**
 * @flowCustomError
 * @description custom desc
 */
type Person = {
  name: string,
  age: number,
  ...
};

const personTest1: Person = { name: "Alice", age: 30 }; // OK
const personTest2: Person = { name: "Bob" }; // Error: Should show custom error
const personTest3: Person = 42; // Error: Should show custom error

// Test 4: Type alias with @flowCustomError for union types
/**
 * @flowCustomError
 * @description custom desc
 */
type Status = "pending" | "approved" | "rejected";

const statusTest1: Status = "pending"; // OK
const statusTest2: Status = "invalid"; // Error: Should show custom error

// Test 5: Type alias with @flowCustomError for intersection types
/**
 * @flowCustomError
 * @description custom desc
 */
type Employee = Person & { employeeId: number, ... };

const employeeTest1: Employee = { name: "Charlie", age: 25, employeeId: 12345 }; // OK
const employeeTest2: Employee = { name: "Diana", age: 28 }; // Error: Should show custom error
const employeeTest3: Employee = 42; // Error: Should show custom error

// Test 6: Type alias with @flowCustomError for function types
/**
 * @flowCustomError
 * @description custom desc
 */
type ValidatorFn = (value: string) => boolean;

const validatorTest1: ValidatorFn = (x) => x === ""; // OK
const validatorTest2: ValidatorFn = 42; // Error: Should show custom error

// Test 7: Type alias with @flowCustomError for array types
/**
 * @flowCustomError
 * @description custom desc
 */
type UserList = Array<UserId>;

const userListTest1: UserList = ["user1", "user2"]; // OK
const userListTest2: UserList = [1, 2, 3]; // Error: Should show custom error for array elements

// Test 8: Normal type alias for comparison (no custom error)
type RegularNumber = number;

const regularTest1: RegularNumber = "not a number"; // Error: Normal error message
const regularTest2: RegularNumber = 123; // OK

// Test 9: Nested custom error types
/**
 * @flowCustomError
 * @description custom desc
 */
type Department = {
  name: string,
  manager: Employee,
};

const deptTest1: Department = {
  name: "Engineering",
  manager: { name: "Eve", age: 35, employeeId: 54321 }
}; // OK
const deptTest2: Department = {
  name: "Marketing",
  manager: { name: "Frank", age: 40 } // Error: Should show custom error
};
const deptTest3: Department = "not an object"; // Error: Should show custom error

// Test 10: Type alias with optional properties
/**
 * @flowCustomError
 * @description custom desc
 */
type OptionalUser = {
  id: UserId,
  nickname?: string,
};

const optUserTest1: OptionalUser = { id: "user456" }; // OK
const optUserTest2: OptionalUser = { id: "user789", nickname: "nick" }; // OK
const optUserTest3: OptionalUser = { nickname: "only_nick" }; // Error: Should show custom error
const optUserTest4: OptionalUser = 42; // Error: Should show custom error

// Test 11: Generic type alias with @flowCustomError
/**
 * @flowCustomError
 * @description custom desc
 */
type Box<T> = {
  value: T,
  label: string,
};

const boxTest1: Box<number> = { value: 42, label: "number box" }; // OK
const boxTest2: Box<string> = { value: "hello", label: "string box" }; // OK
const boxTest3: Box<number> = { value: "not a number", label: "wrong" }; // Error: Should show custom error
const boxTest4: Box<string> = 42; // Error: Should show custom error

// Test 12: Generic type with multiple type parameters
/**
 * @flowCustomError
 * @description custom desc
 */
type Pair<A, B> = {
  first: A,
  second: B,
};

const pairTest1: Pair<number, string> = { first: 1, second: "one" }; // OK
const pairTest2: Pair<string, number> = { first: "one", second: 1 }; // OK
const pairTest3: Pair<number, string> = { first: "wrong", second: "one" }; // Error: Should show custom error
const pairTest4: Pair<string, number> = { first: "one", second: "wrong" }; // Error: Should show custom error
const pairTest5: Pair<number, string> = 42; // Error: Should show custom error

// Test 13: Generic type with bounded type parameters
/**
 * @flowCustomError
 * @description custom desc
 */
type Container<T: string | number> = {
  items: Array<T>,
  count: number,
};

const containerTest1: Container<string> = { items: ["a", "b"], count: 2 }; // OK
const containerTest2: Container<number> = { items: [1, 2, 3], count: 3 }; // OK
const containerTest3: Container<string> = { items: [1, 2], count: 2 }; // Error: Should show custom error
const containerTest4: Container<number> = "not a container"; // Error: Should show custom error

// Test 14: Generic type with default type parameters
/**
 * @flowCustomError
 * @description custom desc
 */
type Result<T, E = string> =
  | { type: "success", value: T }
  | { type: "error", error: E };

const resultTest1: Result<number> = { type: "success", value: 42 }; // OK
const resultTest2: Result<number> = { type: "error", error: "failed" }; // OK
const resultTest3: Result<number> = { type: "success", value: "wrong" }; // Error: Should show custom error
const resultTest4: Result<number> = 42; // Error: Should show custom error

// Test 15: Nested generic types with custom errors
/**
 * @flowCustomError
 * @description custom desc
 */
type Wrapper<T> = Box<T>;

const wrapperTest1: Wrapper<number> = { value: 42, label: "wrapped" }; // OK
const wrapperTest2: Wrapper<number> = { value: "not a number", label: "wrong" }; // Error: Should show custom error
const wrapperTest3: Wrapper<number> = 42; // Error: Should show custom error

// Test 16: Generic type alias of array
/**
 * @flowCustomError
 * @description custom desc
 */
type List<T> = Array<T>;

const listTest1: List<number> = [1, 2, 3]; // OK
const listTest2: List<string> = ["a", "b", "c"]; // OK
const listTest3: List<number> = ["not", "numbers"]; // Error: Should show custom error
const listTest4: List<string> = 42; // Error: Should show custom error

// Test 17: Generic type with function types
/**
 * @flowCustomError
 * @description custom desc
 */
type Mapper<T, U> = (input: T) => U;

const mapperTest1: Mapper<number, string> = (x) => x; // Error: Should show custom error
const mapperTest2: Mapper<number, string> = 42; // Error: Should show custom error

// Test 18: Generic type with complex nested structures
/**
 * @flowCustomError
 * @description custom desc
 */
type Tree<T> = {
  value: T,
  left: ?Tree<T>,
  right: ?Tree<T>,
};

const treeTest1: Tree<number> = {
  value: 1,
  left: { value: 2, left: null, right: null },
  right: null,
}; // OK
const treeTest2: Tree<number> = {
  value: 1,
  left: { value: "wrong", left: null, right: null }, // Error: Should show custom error
  right: null,
};
const treeTest3: Tree<string> = 42; // Error: Should show custom error

// Test 19: Generic type with variance annotations
/**
 * @flowCustomError
 * @description custom desc
 */
type ReadOnlyBox<+T> = {
  +value: T,
  +label: string,
};

const roBoxTest1: ReadOnlyBox<number> = { value: 42, label: "readonly" }; // OK
const roBoxTest2: ReadOnlyBox<number | string> = { value: 42, label: "covariant" }; // OK due to covariance
const roBoxTest3: ReadOnlyBox<number> = { value: "wrong", label: "error" }; // Error: Should show custom error
const roBoxTest4: ReadOnlyBox<number> = 42; // Error: Should show custom error

// Test 20: Normal generic type (no custom error) for comparison
type NormalBox<T> = {
  value: T,
};

const normalBoxTest1: NormalBox<number> = { value: 42 }; // OK
const normalBoxTest2: NormalBox<number> = { value: "wrong" }; // Error: Normal error message
const normalBoxTest3: NormalBox<number> = 42; // Error: Normal error message
