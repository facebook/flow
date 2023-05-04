export function isStringNullOrEmpty(str: ?string): boolean %checks {
  return str == null || str === "";
}

declare export function declaredIsStringNullOrEmpty(
  str: ?string
): boolean %checks(str == null || str === "");

const y = "abc";

declare function mono(x: mixed, y: mixed): boolean %checks(typeof x === "number");

export function wrapMono(x: mixed): boolean %checks {
  return mono(x, y);
}
