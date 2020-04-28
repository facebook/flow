// @flow

enum E {
  A,
  B,
}

enum F {
  A,
  B,
}

const x = E.A;

///////////
// Valid //
///////////
switch (x)  {
  case E.A: break;
  case E.B: break;
}

switch (x)  {
  case E.A: break;
  default: break;
}

const e = E;
switch (x)  {
  case e.A: break;
  case e.B: break;
}

////////////
// Errors //
////////////

// Missing check
switch (x)  { // Error
  case E.A: break;
}

// Invalid check
switch (x)  {
  case x: // Error
}

enum G {}
function g(g: G) {
  switch (g)  {
    case g: break; // Error
  }
}

switch (x)  {
  case E.isValid: break; // Error
}

// Duplicate check
switch (x)  {
  case E.A: break;
  case E.B: break;
  case E.A: break; // Error
}

switch (x)  {
  case E.A: break;
  case E.B: break;
  default: // Error
}

// Incompatible types
switch (x)  { // Error
  case F.A: break; // Error
  case F.B: break;
}

declare var s: string;
switch (s)  {
  case E.A: break; // Error
  case E.B: break; // Error
}

// Discriminant is union
function f1(x?: E) {
  switch (x)  {
    case E.A: break; // Error
    case E.B: break; // Error
  }
}

function f2(x: ?E) {
  switch (x)  {
    case E.A: break; // Error
    case E.B: break; // Error
  }
}

function f3(x: ?E) {
  switch (x)  { // Error
    case E.A: break; // Error
  }
}

function f4(x: E | F) {
  switch (x)  { // Error
    case E.A: break; // Error
    case E.B: break; // Error
  }
}

function f5(x: E | string) {
  switch (x)  {
    case E.A: break; // Error
    case E.B: break; // Error
  }
}

switch (x) { // Error
  case E.A: break;
  case F.A: break; // Error
}

function f6(x: E | E) {
  switch (x) {
    case E.A: break;
    case E.B: break;
  }

  switch (x) { // Error
    case E.A: break;
  }
}

function f7(x: E & E) {
  switch (x) {
    case E.A: break;
    case E.B: break;
  }

  switch (x) { // Error
    case E.A:
  }
}
