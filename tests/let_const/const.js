function basic(): string {
  const foo = "foo";
  return foo;
}

function isBlockScoped(): string {
  const foo = "foo";
  { const foo = 1 };
  return foo;
}
