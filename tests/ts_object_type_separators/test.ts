// Type literals retain newline-separated required and optional properties.
type Shape = {
  count: number // OK
  name?: string // OK
}
const shape: Shape = {count: 1}; // OK
shape.count satisfies number; // OK
shape.count satisfies string; // ERROR

// Interface method and readonly-property members are checked normally.
interface Reader {
  read(): number // OK
  readonly name: string // OK
}
const reader: Reader = {read() { return 1; }, name: 'reader'}; // OK
reader.read() satisfies number; // OK

// Named index signatures remain dictionaries when separated by a newline.
type Dictionary = {
  [key: string]: number // OK
  size: number // OK
}
const dictionary: Dictionary = {size: 1, other: 2}; // OK
dictionary.other satisfies number; // OK

// Ordinary .ts files remain implementation files, not file-wide ambient declarations.
function twice(n: number): number { return n * 2; } // OK
twice(shape.count) satisfies number; // OK
