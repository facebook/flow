function singleAssignment() {
  const foo = "foo"
  foo = "bar" // error
}

function singleDeclaration() {
  const foo = "foo"
  const foo = "bar" // error
}
