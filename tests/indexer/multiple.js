let tests = [
  function() {
    ({} as {
      [k1: string]: string,
      [k2: number]: number, // error: not supported (yet)
    });
  }
];
