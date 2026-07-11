type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "square"; side: number }
  | { kind: "rectangle"; width: number; height: number };

function assertNever(value: never): never {
  throw new Error(`Unhandled shape: ${JSON.stringify(value)}`);
}

function area(shape: Shape): number {
  switch (shape.kind) {
    case "circle":
      return Math.PI * shape.radius ** 2;
    case "square":
      return shape.side * shape.side;
    case "rectangle":
      return shape.width * shape.height;
    default:
      return assertNever(shape);
  }
}

function fail(message: string): never {
  throw new Error(message);
}

function parsePositive(input: string): number {
  const n = Number(input);
  if (Number.isNaN(n) || n <= 0) {
    fail(`Not a positive number: ${input}`);
  }
  return n;
}

const shapes: Shape[] = [
  { kind: "circle", radius: 2 },
  { kind: "square", side: 3 },
  { kind: "rectangle", width: 2, height: 5 },
];

for (const shape of shapes) {
  console.log(area(shape));
}
console.log(parsePositive("42"));
