declare let a: (number) => unknown;
declare let b: (number) => unknown;
declare let c: (number) => unknown;
declare let d: (number) => unknown;

[a, [b, [c, {d}]]] = [
   (n) => n as empty, // error: num ~> empty
   [
      (n) => n as empty, // error: num ~> empty
      [
        (n) => n as empty, // error: num ~> empty
        {d: (n) => n as empty} // error: num ~> empty
      ]
   ]
];
({a, b} = {
  a: (n) => n as empty, // error: num ~> empty
  b: (n) => n as empty, // error: num ~> empty
})

declare let e: ReadonlyArray<(number) => unknown>;
declare let f: {[string]: (number) => unknown};
[...e] = [(n) => n as empty]; // error: num ~> empty
({...f} = {hello: (n) => n as empty}); // error: num ~> empty

[, b] = [1, (n) => {}]; // error: missing-local-annot (array hole pattern is unsupported)
