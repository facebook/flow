// @flow

{
  const { ParentFoo } = require('./Parent');
  ParentFoo;
//   ^
}

{
  let Parent = require('./Parent');
//     ^
}

{
  let Parent = require('./Parent');
//                          ^
}

// Follows non-destructured property access of `require('Parent')`
{
  let foo = require('./Parent').ParentFoo.foo;
//     ^
  foo;
// ^

  let bar = require('./Parent').ParentFoo.foo;
//                                         ^
}

// Chaining `require`s through multiple files
{
  var Child = require('./Child');
  Child.ChildFoo.foo;
//        ^
  Child.ChildFoo.foo;
//                ^
}

// Shadowing `require`
{
  {
    let require = (x : string) => x === './Parent';
    let adopted = require('./Parent');
//       ^
    adopted;
//     ^
  }
  // No longer shadowed
  let Parent = require('./Parent');
//     ^
}
