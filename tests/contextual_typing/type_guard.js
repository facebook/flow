const fn: (x: number | string) => x is string = (y): y is string => {
  (y: number | string); // okay
  (y: empty); // error number | string ~> empty
  return typeof y === 'string';
};
