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

function functionsAreLexicalToo(): [string, string] {
  let foo = "foo";
  const bar = "bar";
  function inner() {
    let foo = 1;
    const bar = 2;
  }
  return [foo, bar];
}

function varsStillHoist(): string {
  var foo = 1;
  {
    var foo = "foo";
  }
  return foo;
}

function TDZ1() {
  console.log(foo, bar, baz); // ReferenceErrors (baz is OK)
  let foo = 1;
  const bar = 2;
  var baz = 3;
}

function TDZ2() {
  {
    console.log(foo, bar, baz); // ReferenceErrors (baz is OK)
  }
  let foo = 1;
  const bar = 2;
  var baz = 3;
}

function TDZ3() {
  let foo = 1;
  const bar = 2;
  var baz = 3;
  {
    console.log(foo, bar, baz); // ReferenceErrors (baz is OK)
    let foo = 1;
    const bar = 2;
    var baz = 3;
  }
}

function TDZ4() {
  function fn() {
    console.log(foo, bar, baz); // OK: only called after assignment
  }
  let foo = 1;
  const bar = 2;
  var baz = 3;
  fn();
}

function TDZ5() {
  function fn() {
    console.log(foo, bar, baz); // TODO: called before assignment, should fail
  }
  fn();
  let foo = 1;
  const bar = 2;
  var baz = 3;
}
