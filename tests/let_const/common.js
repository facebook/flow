function basic(): [string, string] {
  let foo = "foo";
  const bar = "bar";
  return [foo, bar];
}

function isBlockScoped(): [string, string] {
  let foo = "foo";
  const bar = "bar";
  {
    let foo = 1;
    const bar = 2;
  }
  return [foo, bar];
}

/* TODO: function TDZ1() {
  console.log(foo, bar, baz); // ReferenceErrors (baz is OK)
  let foo = 1;
  const bar = 2;
  var baz = 3;
} */
