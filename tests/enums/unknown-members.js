// @flow

enum E {
  A,
  B,
  ...
}

const x: E = E.A;

///////////
// Valid //
///////////
switch (x)  {
  case E.A: break;
  case E.B: break;
  default: break;
}

switch (x)  {
  case E.A: break;
  default: break;
}

////////////
// Errors //
////////////

// Missing check
switch (x)  { // Error
  case E.A: break;
}

// Duplicate check
switch (x)  { // Error
  case E.A: break;
  case E.B: break;
  case E.A: break; // Error
}

// Has unknown members, so always needs a `default`
switch (x)  { // Error
  case E.A: break;
  case E.B: break;
}
