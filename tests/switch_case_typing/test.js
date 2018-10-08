// @flow

type Enum = 'foo' | 'bar' | 'baz';
declare var x: Enum;

switch (x) {
  case 'foo':
  case 'bar':
    break;
  case 'qux': // error
    break;
}

switch (x) {
  case 'foo':
  case 'bar':
  case 'baz':
    break;
  case 3: // error
    break;
}

switch (x) {
  case 'foo':
  case 'bar':
  case 'baz':
    break;
  case 'qux': // error
    break;
}

if (x === 'foo') {}
else if (x === 'bar') {}
else if (x === 'baz') {}
else if (x === 'qux') {} // error

if (x === 'foo') {}
else if (x === 'bar') {}
else if (x === 'qux') {} // error

type eNum = 1 | 2 | 3;
declare var y: eNum;

switch (y) {
  case 1:
  case 2:
  case 4: // error
    break;
}

switch (y) {
  case 1:
  case 2:
  case 3:
    break;
  case 4: // error
    break;
}

type DisjointUnion = { type: 'foo' } | { type: 'bar' } | { type: 'baz' }
declare var o: DisjointUnion;

switch (o.type) {
  case 'foo':
  case 'bar':
    break;
  case 'qux': // error
    break;
}

switch (o.type) {
  case 'foo':
  case 'bar':
  case 'baz':
    break;
  case 'qux': // error
    break;
}

switch (o.type) {
  case 'foo':
  case 'bar':
  case 'baz':
    break;
  case 3: // error
    break;
}
