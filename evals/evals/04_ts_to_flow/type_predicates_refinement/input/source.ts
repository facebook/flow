function isString(value: unknown): value is string {
  return typeof value === "string";
}

function isNumber(value: unknown): value is number {
  return typeof value === "number";
}

interface Partitioned {
  strings: string[];
  numbers: number[];
  other: number;
}

function partition(values: unknown[]): Partitioned {
  const strings = values.filter(isString);
  const numbers = values.filter(isNumber);
  const other = values.length - strings.length - numbers.length;
  return { strings, numbers, other };
}

function joinStrings(values: unknown[], separator: string): string {
  return values.filter(isString).join(separator);
}

const mixed: unknown[] = ["a", 1, "b", true, 2, null, "c"];
const result = partition(mixed);

console.log(result.strings, result.numbers, result.other);
console.log(joinStrings(mixed, ", "));
