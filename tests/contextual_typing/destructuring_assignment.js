declare var a: (number) => unknown;
declare var b: (number) => unknown;
declare var c: (number) => unknown;
declare var d: (number) => unknown;

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

declare var e: $ReadOnlyArray<(number) => unknown>;
declare var f: {[string]: (number) => unknown};
[...e] = [(n) => n as empty]; // error: num ~> empty
({...f} = {hello: (n) => n as empty}); // error: num ~> empty

[, b] = [1, (n) => {}]; // error: missing-local-annot (array hole pattern is unsupported)
