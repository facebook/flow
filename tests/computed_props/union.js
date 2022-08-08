//@flow
type key = 'a' | 'b' | 'c';

declare var k: key;
const a = {[k]: 3}; // Error, k has multiple lower bounds


let x;
if (true) {
  x = 'foo';
} else if (true) {
  x = 'bar';
} else {
  x = 'baz';
}

const b = {[x]: 3} // Error, x has multiple lower bounds

declare var maybe: ?string;
const c = {[maybe]: 3}; // ERROR

declare var several: ?string | key;
const d = {[several]: 3}; // ERROR

declare var str: string;
const nested = {
  [str]: {
    [several]: 3, // ERROR
  },
};
