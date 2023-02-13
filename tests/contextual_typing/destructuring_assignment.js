declare var a: (number) => mixed;
declare var b: (number) => mixed;
declare var c: (number) => mixed;
declare var d: (number) => mixed;

[a, [b, [c, {d}]]] = [
   (n) => (n: empty), // error: num ~> empty
   [
      (n) => (n: empty), // error: num ~> empty
      [
        (n) => (n: empty), // error: num ~> empty
        {d: (n) => (n: empty)} // error: num ~> empty
      ]
   ]
];
({a, b} = {
  a: (n) => (n: empty), // error: num ~> empty
  b: (n) => (n: empty), // error: num ~> empty
})

declare var e: Array<(number) => mixed>;
declare var f: {[string]: (number) => mixed};
[...e] = [(n) => (n: empty)]; // error: num ~> empty
({...f} = {hello: (n) => (n: empty)}); // error: num ~> empty

[, b] = [1, (n) => {}]; // error: missing-local-annot (array hole pattern is unsupported)
