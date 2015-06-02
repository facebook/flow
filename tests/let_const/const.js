function singleAssignment() {
  const foo = "foo"
  foo = "bar" // error
}

function singleDeclaration() {
  const foo = "foo"
  const foo = "bar" // error
}

function forInLoop(): number {
  const prop = 1;
  let obj = { foo: true }; // TODO: this can't be const

  for (const prop in obj) {
    const val: boolean = obj[prop];
  }

  return prop;
}

function forOfLoop(): number {
  const x = 1;

  for (const x of "abc") {}

  return x;
}
