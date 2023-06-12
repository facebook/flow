// We need to be careful to not look for component return expressions
// while verifying a function component and vice-versa

function f(): number {
  component Foo() renders string {
    return 'str'; // No error
  }
  return 3;
}

component Foo() renders number {
  function f(): string {
    return 'str'; // No error
  }
  return 3;
}
