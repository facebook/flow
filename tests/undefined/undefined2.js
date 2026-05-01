let tests = [
  function(x: number) {
    var id;
    var name = id ? 'John' : undefined; // error: id is unitialized, so constant-condition
    name as boolean; // error, string or void

    const bar = [
      undefined,
      'bar',
    ];
    bar[x] as boolean; // error, string or void
  },

  function(x: number) {
    var undefined = 6;
    undefined as number; // ok

    var x;
    if (x !== undefined) {
      x[0]; // error, no property 0
    }

    const bar = [
      undefined,
      'bar',
    ];
    bar[x] as boolean; // error, string only
  },
];
