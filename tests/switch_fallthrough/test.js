type Enum = 'foo' | 'bar' | 'baz';
declare var x: Enum;
switch (x) {
  case 'bar':
    break;
}

(x : 'bar'); // error

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
    break;
  case 'qux': // error
    break;
}

switch (x) {
  case 'bar':
    x = 'foo';
}

(x : 'foo'); // error

switch (x) {
  case 'bar':
  case 'baz':
    x = 'foo';
}

(x : 'foo'); // no error
