let esc1; // error
function a<T>() {
  esc1 = 1;
}
(esc1: void);

let esc2; // error
function b<T>() {
  esc2 = 1;
}
function c<T>() {
  esc2 = 2;
}
(esc2: void);

let esc3 = null; // error
function d<T>() {
  esc3 = 1;
}
(esc3: null);

let esc4 = null; // error
function e<T>() {
  esc4 = 1;
}
function f<T>() {
  esc4 = 2;
}
(esc4: null);
