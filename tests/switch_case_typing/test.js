type Enum = 'foo' | 'bar' | 'baz';
{
  declare const x: Enum;
  switch (x) {
    case 'foo':
    case 'bar':
      break;
    case 'qux': // error
      break;
  }
}
{
  declare const x: Enum;
  switch (x) {
    case 'foo':
    case 'bar':
    case 'baz':
      break;
    case 3: // error
      break;
  }
}
{
  declare const x: Enum;
  switch (x) {
    case 'foo':
    case 'bar':
    case 'baz':
      break;
    case 'qux': // error
      break;
  }
}
{
  declare const x: Enum;
  if (x === 'foo') {}
  else if (x === 'bar') {}
  else if (x === 'baz') {}
  else if (x === 'qux') {} // error
}
{
  declare const x: Enum;
  if (x === 'foo') {}
  else if (x === 'bar') {}
  else if (x === 'qux') {} // error
}

type eNum = 1 | 2 | 3;
{
  declare const y: eNum;
  switch (y) {
    case 1:
    case 2:
    case 4: // error
      break;
  }
}
{
  declare const y: eNum;
  switch (y) {
    case 1:
    case 2:
    case 3:
      break;
    case 4: // error
      break;
  }
}

type DisjointUnion = { type: 'foo' } | { type: 'bar' } | { type: 'baz' }
{
  declare const o: DisjointUnion;
  switch (o.type) {
    case 'foo':
    case 'bar':
      break;
    case 'qux': // error
      break;
  }
}
{
  declare const o: DisjointUnion;
  switch (o.type) {
    case 'foo':
    case 'bar':
    case 'baz':
      o.type as empty; // error
      break;
    case 'qux': // error
      o.type as empty; // error
      break;
  }
}
{
  declare const o: DisjointUnion;
  switch (o.type) {
    case 'foo':
    case 'bar':
    case 'baz':
      break;
    case 3: // error
      break;
  }
}

{
  declare const s: String;
  if (s === "a" || s === "b") {} // error
  if (s === "a" && s === "b") {} // error
}

{
  declare var st: string;
  if (st === 1 || st === 2) {} // error
  if (st === 1 && st === 2) {} // error
}

{
  type Obj = { field : string };
  declare var obj: Obj;
  if (obj.field === 1 || obj.field === 2) {} // error
  if (obj.field === 1 && obj.field === 2) {} // error
}

const KeyObj = {
  "FOO": 'foo',
  "BAR": 'bar',
  "ERR": 'err',
} as const;
{
  declare const o: DisjointUnion;
  if (o.type === KeyObj.ERR) {} // error
  if (KeyObj.ERR === o.type) {} // error
}
{
  declare const o: DisjointUnion;
  switch (o.type) {
    case KeyObj.FOO:
    case KeyObj.BAR:
    case KeyObj.ERR: // error
      break;
    default:
      break;
  }
}

{
  // We relax the inclusion requirement for object literals
  const P = {
    field: 'a',
  }

  switch (P.field) {
    case 'a':
      break;
    case 'b': // okay
      break;
    default:
      break;
  }
}
