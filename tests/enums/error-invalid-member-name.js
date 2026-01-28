enum E {
  foo, // ERROR
  bar, // ERROR
  Good, // OK
}

enum F of string {
  lowercase = "value", // ERROR
  Uppercase = "value2", // OK
}

enum G of number {
  bad = 1, // ERROR
  Good = 2, // OK
}

enum H of boolean {
  yes = true, // ERROR
  No = false, // OK
}
