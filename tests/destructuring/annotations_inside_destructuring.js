// Annotation on nested identifier
{
  const x: [number, string] = [1, "hi"];
  const [a: boolean, b: boolean] = x; // ERROR
}
{
  const x: {a: number, b: string} = {a: 1, b: "hi"};
  const {a: a: boolean, b: b: boolean} = x; // ERROR
}

// Annotation on nested object
{
  const x: [{a: number, b: string}] = [{a: 1, b: "hi"}];
  const [{a, b}: {a: boolean, b: boolean}] = x; // ERROR
}

// Annotation on nested tuple
{
  const x: {k: [number, string]} = {k: [1, "hi"]};
  const {k: [a, b]: [boolean, boolean]} = x; // ERROR
}

// Annotation on rest element
{
  const x: Array<number> = [1, 2];
  const [...xs: Array<number>] = x;
}
