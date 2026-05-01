const fn: (x: number | string) => x is string = (y): y is string => {
  y as number | string; // okay
  y as empty; // error number | string ~> empty
  return typeof y === 'string';
};
