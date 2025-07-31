let tests = [
  function(x: number) {
    var id;
    var name = id ? 'John' : undefined;
    (name: boolean); // error, string or void

    const bar = [
      undefined,
      'bar',
    ];
    (bar[x]: boolean); // error, string or void
  },

  function(x: number) {
    var undefined = 6;
    (undefined: number); // ok

    var x;
    if (x !== undefined) {
      x[0]; // error, no property 0
    }

    const bar = [
      undefined,
      'bar',
    ];
    (bar[x]: boolean); // error, string only
  },
];
