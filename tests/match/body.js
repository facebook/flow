// Type of body and abnormal handling
{
  declare const x: empty;
  const out = match (x) {};

  out as empty; // OK
}

declare const x: 1 | 2;

{
  const out = match (x) {
    1: true,
    2: false,
  };

  out as boolean; // OK
  out as empty; // ERROR
}

{
  const out = match (x) {
    1: true,
    2: 's',
  };
  out as boolean | string; // OK
  out as empty; // ERROR
}

declare function invariant(boolean): empty;

{
  const out = match (x) {
    1: true,
    2: invariant(false),
  };
  out as boolean; // OK
  out as empty; // ERROR
}

function f1() {
  const out = match (x) {
    1: invariant(false),
    2: invariant(false),
  };
  out; // ERROR: unreachable
}

// Throws in guards
function f2() {
  const out = match (x) {
    1 if invariant(false): true,
    _: 's',
  };
  out as string; // OK
  out as empty; // ERROR
}

// Nested matches
{
  const out = match (x) {
    1: 1,
    const a: match (a) {
      const a: a,
    },
  };

  out as number; // OK
}

// Guards can refine values which are not the argument
{
  declare const y: number | string;

  const out = match (x) {
    1 if typeof y === 'number': y as number, // OK
    const a if a === 1: a as 1,
    _: 0,
  };
}
