// @flow

import type {
  Email,
  Age,
  UserProfile,
  ValidatedInput,
  ApiResponse,
  ID,
  NormalType,
  Coordinate,
  Point3D,
  Color,
  ColorMap,
  Callback,
  Optional,
  ReadonlyRecord,
} from './exported';

// Test 1: Imported primitive custom error types
const emailTest1: Email = "user@example.com"; // OK
const emailTest2: Email = 123; // Error: TODO: should show custom error, but doesn't since comment is not properly attached.

const ageTest1: Age = 25; // OK
const ageTest2: Age = "twenty-five"; // Error: Should show custom error

// Test 2: Imported object type with custom error
const profileTest1: UserProfile = {
  email: "alice@example.com",
  age: 30,
  displayName: "Alice"
}; // OK

const profileTest2: UserProfile = { // Error: age should show custom error
  email: "bob@example.com",
  age: "thirty",
  displayName: "Bob"
};

const profileTest3: UserProfile = { // Error: email should show custom error
  email: 123,
  age: 25,
  displayName: "Charlie"
};

const profileTest4: UserProfile = "not an object"; // Error: Should show custom error

// Test 3: Imported generic type with custom error
const validatedTest1: ValidatedInput<string> = {
  value: "hello",
  isValid: true,
  errors: []
}; // OK

const validatedTest2: ValidatedInput<number> = { // Error: Should show custom error
  value: "not a number",
  isValid: false,
  errors: ["Invalid number"]
};

const validatedTest3: ValidatedInput<string> = 42; // Error: Should show custom error

// Test 4: Imported generic type with default parameter
const apiTest1: ApiResponse<string> = {
  status: "success",
  data: "result"
}; // OK

const apiTest2: ApiResponse<string> = {
  status: "error",
  error: "Something went wrong"
}; // OK

const apiTest3: ApiResponse<string> = { // Error: Should show custom error
  status: "success",
  data: 123
};

const apiTest4: ApiResponse<string> = "not a response"; // Error: Should show custom error

// Test 5: Imported union type with custom error
const idTest1: ID = "abc123"; // OK
const idTest2: ID = 42; // OK
const idTest3: ID = true; // Error: Should show custom error

// Test 6: Normal imported type (no custom error) for comparison
const normalTest1: NormalType = { field: "value" }; // OK
const normalTest2: NormalType = { field: 123 }; // Error: Normal error message
const normalTest3: NormalType = "not an object"; // Error: Normal error message

// Test 7: Imported intersection type with custom error
const pointTest1: Point3D = { x: 1, y: 2, z: 3 }; // OK
const pointTest2: Point3D = { x: 1, y: 2 }; // Error: Should show custom error
const pointTest3: Point3D = "not a point"; // Error: Should show custom error

// Test 8: Imported literal union type with custom error
const colorTest1: Color = "red"; // OK
const colorTest2: Color = "green"; // OK
const colorTest3: Color = "yellow"; // Error: Should show custom error

// Test 9: Imported generic object map with custom error
const colorMapTest1: ColorMap<number> = {
  red: 255,
  green: 128,
  blue: 64
}; // OK

const colorMapTest2: ColorMap<string> = {
  red: "#FF0000",
  green: "#00FF00",
  blue: "#0000FF"
}; // OK

const colorMapTest3: ColorMap<number> = { // Error: Should show custom error
  red: 255,
  green: "not a number",
  blue: 64
};

const colorMapTest4: ColorMap<number> = "not a color map"; // Error: Should show custom error

// Test 10: Imported function type with custom error
const callbackTest1: Callback<number> = (x) => {}; // OK
const callbackTest2: Callback<string> = (x) => {}; // OK
const callbackTest3: Callback<number> = "not a function"; // Error: Should show custom error

// Test 11: Imported optional type with custom error
const optionalTest1: Optional<number> = 42; // OK
const optionalTest2: Optional<number> = null; // OK
const optionalTest3: Optional<number> = undefined; // OK
const optionalTest4: Optional<number> = "not a number"; // Error: Should show custom error

// Test 12: Imported readonly record with custom error
const recordTest1: ReadonlyRecord<string, number> = {
  foo: 1,
  bar: 2,
  baz: 3
}; // OK

const recordTest2: ReadonlyRecord<string, string> = {
  name: "Alice",
  email: "alice@example.com"
}; // OK

const recordTest3: ReadonlyRecord<string, number> = { // Error: Should show custom error
  foo: 1,
  bar: "not a number"
};

const recordTest4: ReadonlyRecord<string, number> = "not a record"; // Error: Should show custom error

// Test 13: Using imported types with local custom error types
/**
 * @flowCustomError
 */
type EnrichedProfile = {
  profile: UserProfile,
  lastLogin: number,
  preferences: ReadonlyRecord<string, string>,
};

const enrichedTest1: EnrichedProfile = {
  profile: {
    email: "user@example.com",
    age: 30,
    displayName: "User"
  },
  lastLogin: 0,
  preferences: { theme: "dark", language: "en" }
}; // OK

const enrichedTest2: EnrichedProfile = { // Error: Should show custom error for EnrichedProfile
  profile: {
    email: 123,
    age: 30,
    displayName: "User"
  },
  lastLogin: 0,
  preferences: { theme: "dark" }
};

const enrichedTest3: EnrichedProfile = "not an enriched profile"; // Error: Should show custom error for EnrichedProfile
