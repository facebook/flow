// We need to be careful to not look for component return expressions
// while verifying a function component and vice-versa

function f(): number {
  component Foo(): string {
    return 'str'; // TODO: No error
  }
  return 3;
}
