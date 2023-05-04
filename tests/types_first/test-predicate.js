import {
  isStringNullOrEmpty,
  declaredIsStringNullOrEmpty,
  wrapMono,
} from "./exports-predicate";

declare var s: null | string;

if (!isStringNullOrEmpty(s)) {
  (s: string);
}

if (!declaredIsStringNullOrEmpty(s)) {
  (s: string);
}

declare var x: mixed;
if (wrapMono(x)) {
  (x: number); // okay
  (x: string); // error
}
