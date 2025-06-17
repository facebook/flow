// Unnecessary basic pattern: seen pattern before
{
  declare const x: 1 | 2;

  match (x) {
    1 => {}
    2 => {}
    1 => {} // ERROR: seen before
  }
}

// Unnecessary basic pattern: seen wildcard
{
  declare const x: 1 | 2;

  match (x) {
    1 => {}
    _ => {}
    2 => {} // ERROR: seen wildcard
  }
}

// Unnecessary wildcard: seen wildcard
{
  declare const x: 1 | 2;

  match (x) {
    1 => {}
    _ => {}
    _ => {} // ERROR: already seen wildcard
  }

  match (x) {
    1 => {}
    const a => {}
    const a => {} // ERROR: already seen wildcard
  }

  match (x) {
    1 => {}
    _ => {}
    const a => {} // ERROR: already seen wildcard
  }

  match (x) {
    1 => {}
    const a => {}
    _ => {} // ERROR: already seen wildcard
  }
}

// Invalid identifier pattern
{
  declare const num: number;

  declare const x: number;

  match (x) {
    num => {}
  }
}

// Invalid member pattern
{
  declare const obj: {
    num: number,
  };

  declare const x: number;

  match (x) {
    obj.num => {}
  }
}
