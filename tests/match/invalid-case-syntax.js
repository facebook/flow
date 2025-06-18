declare const x: 0;

// Statement
{
  match (x) {
    case 0 => {} // ERROR
  }

  match (x) {
    0: {} // ERROR
  }
}

// Expression
{
  const e = match (x) {
    case 0 => 1, // ERROR
  };
}
{
  const e = match (x) {
    0: 1, // ERROR
  };
}
{
  const e = match (x) {
    0 => 1; // ERROR
  };
}

// Many errors, but one kind
{
  declare const x: 1 | 2 | 3;

  match (x) { // ERROR
    case 1 => {}
    case 2 => {}
    case 3 => {}
  }

  const e = match (x) { // ERROR
    1 => null,
    2 => null,
    3 => null,
  };
}

// Many errors, multiple kinds
{
  declare const x: 1 | 2 | 3;

  match (x) { // ERROR
    case 1: {}
    case 2: {}
    case 3: {}
  }

  const e = match (x) { // ERROR
    case 1: null;
    case 2: null;
    case 3: null;
  };
}
