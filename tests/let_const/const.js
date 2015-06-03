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
  const obj = { foo: true };

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

// Ensure refinements aren't redefinition errors
function refi(): number {
  const x: ?number = 1;
  if (x) {
    return x;
  }
  return 1;
}
