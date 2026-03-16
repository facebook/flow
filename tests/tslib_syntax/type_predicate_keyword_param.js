declare function isType(type: unknown): type is string;

declare const x: unknown;
if (isType(x)) {
  x as string;
}
