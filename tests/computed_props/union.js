//@flow
type key = 'a' | 'b' | 'c';

declare var k: key;
const a = {[k]: 3}; // multiple lower bounds okay
a as {}; // error

let x;
if (true) {
  x = 'foo';
} else if (true) {
  x = 'bar';
} else {
  x = 'baz';
}

const b = {[x]: 3} // multiple lower bounds okay

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
