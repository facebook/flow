// See [es6_private_abstract.js] for rationale. Field case in its own file
// because Flow surfaces only the first parse error per file.

abstract class B {
  abstract #priv: number; // ERROR: #private+abstract field
}
