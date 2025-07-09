type Enum = 'foo' | 'bar' | 'baz';

{
  declare const x: Enum;
  switch (x) {
    case 'bar':
      break;
  }
  x as 'bar'; // error
}

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
      break;
    case 'qux': // error
      break;
  }
}

{
  declare let x: Enum;
  switch (x) {
    case 'bar':
      x = 'foo';
  }
  x as 'foo'; // error
}

{
  declare let x: Enum;
  switch (x) {
    case 'bar':
    case 'baz':
      x = 'foo';
  }

  x as 'foo'; // no error
}
