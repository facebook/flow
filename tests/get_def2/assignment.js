// @flow

var Parent = require('./Parent');

// Follows assignment on simple/"non-destructuring" patterns
{
  let Parent2;
  Parent2 = Parent;
//   ^
  Parent2; // Points to declaration, not assignment
//   ^
}

{
  let Parent2;
  Parent2 = Parent;
//            ^
}

// Follows assignment with declaration
{
  let Parent3 = Parent;
  Parent3; // Points to LHS of line above this
//   ^
}
