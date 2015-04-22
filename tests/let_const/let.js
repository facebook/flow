//function basic(): string {
//  let foo = "foo";
//  return foo;
//}

function isBlockScoped(): string {
  let foo = "foo";
  { let foo = 1 };
  return foo;
}
