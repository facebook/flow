// @flow
import type {StringMap} from './StringMap';
const input: StringMap = {};

const a: number = input.bar;
if (input.bar) {
  const b:number = input.foo; // should be an error
  const c:number = input.bar; // checked earlier, so okay
}

const {bar} = input;
if (bar) {
  // `input` has maintained its type
  const d:number = input.foo; // error as expected
}
