// Interface with newline-separated properties
interface I {
  x: number
  y: string
  z: boolean
}

// Object type with newline-separated properties
type T = {
  a: number
  b: string
  c: boolean
}

// Mixed separators
type Mixed = {
  a: number,
  b: string;
  c: boolean
  d: number
}

// Optional properties with newline separation
type Opt = {
  x?: number
  y?: string
}

// Method signatures with newline separation
type Methods = {
  foo(): void
  bar(x: number): string
}
