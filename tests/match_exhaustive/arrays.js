// Array
{
  declare const x: Array<number>;

  match (x) { // OK
    [...] => {}
  }

  match (x) { // ERROR
    [_, ...] => {}
  }

  match (x) { // ERROR
    [1, ...] => {} // OK
  }

  match (x) { // ERROR
    [] => {} // OK
  }
}

// $ReadOnlyArray
{
  declare const x: $ReadOnlyArray<number>;

  match (x) { // OK
    [...] => {}
  }

  match (x) { // ERROR
    [_, ...] => {}
  }

  match (x) { // ERROR
    [1, ...] => {} // OK
  }

  match (x) { // ERROR
    [] => {} // OK
  }
}

// Matchable element type
{
  declare const x: Array<1 | 2>;

  match (x) { // OK
    [...] => {}
  }

  match (x) { // ERROR
    [_, ...] => {}
  }

  match (x) { // ERROR
    [1 | 2, ...] => {}
  }

  match (x) { // ERROR
    [1, ...] => {} // OK
  }

  match (x) { // ERROR
    [] => {} // OK
  }
}

// Unnecessary
{
  declare const x: Array<number>;

  match (x) {
    [1, ...] => {} // OK
    [...] => {} // OK
    [1, ...] => {} // ERROR
  }
}

// Arrays are objects
{
  declare const x: Array<boolean>;

  match (x) { // OK
    {...} => {} // OK
  }

  match (x) { // OK
    {length: _, ...} => {} // OK
  }

  match (x) { // OK
    {0: true, ...} => {} // OK
    {1: 'xxx', ...} => {} // ERROR
    [...] => {}
  }
}
