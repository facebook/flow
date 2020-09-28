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

enum Five {
  Apple,
  Banana,
  Cherry,
  Date,
  Elderberry,
}
declare var five: Five;
switch (five) {} // Error

enum Six {
  Apple,
  Banana,
  Cherry,
  Date,
  Elderberry,
  Fig,
}
declare var six: Six;
switch (six) {} // Error
switch (six) { // Error
  case Six.Date: break;
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

// Enum object is a different object
const Fake = {
  A: E.B,
  B: E.A,
}

switch (x) { // Error - two cases below don't count as checks
  case Fake.A: 'actually B!'; break;
  case Fake.B: 'actually A!'; break;
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

// Unions and intersections
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

enum M {
  A,
}
enum N {
  A,
}
function f8(x: M | N, X: typeof M | typeof N) {
  switch (x) { // Error
    case X.A: break; // Error
  }
}

function f9(x: E | F, X: typeof E & typeof F) {
  switch (x) {
    case X.A: break;
    case X.B: break;
  }
}

// Empty
function f10(x: empty) {
  switch (x) {
    case E.A: break;
    case E.B: break;
  }
}

function f11(x: E, X: empty) {
  switch (x) { // Error
    case X.A: break;
  }
}

// Invalid enum member
switch (x) {
  case E.A: break;
  case E.XXX: break; // Error
  case E.B: break;
}
