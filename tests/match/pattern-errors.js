// Match expressions only allow `const` bindings
{
  declare const x: 0 | 1 | [2] | {a: 3, b: 4};

  const e1 = match (x) {
     [...let a]: 0, // ERROR
     {let a, ...let b}: 0, // ERROR
     0 as let a: 0, // ERROR
     let a: 0, // ERROR
  };

  const e2 = match (x) {
     [...var a]: 0, // ERROR
     {var a, ...var b}: 0, // ERROR
     0 as var a: 0, // ERROR
     var a: 0, // ERROR
  };
}
