let tests = [
  function(y: number) {
    y++ as number;
    y-- as number;
    ++y as number;
    --y as number;
  },

  function(y: string) {
    y++; // error, we don't allow coercion here
    y as number; // ok, y is now any
    y as bigint; // ok, y is now any
  },

  function(y: string) {
    y--; // error, we don't allow coercion here
  },

  function(y: string) {
    ++y; // error, we don't allow coercion here
  },

  function(y: string) {
    --y; // error, we don't allow coercion here
  },

  function() {
    const y = 123;
    y++; // error, can't update const
    y--; // error, can't update const
  },

  function(o: {|+y: number|}) {
    o.y++; // error, can't update read-only property
    o.y--; // error, can't update read-only property
  },

  function(o: {|-y: number|}) {
    o.y++; // error, can't read write-only property
    // TODO(T56716039): If you read a write-only property after it is written, there is no error
    // o.y--; // error, can't read write-only property
  },

  function(xs: ReadonlyArray<number>) {
    xs[0]++;
    xs[0]--;
  },

  function(y: any) {
    y++ as number; // ok (because any)
    y++ as bigint; // ok (because any)
  },

  function(y: empty) {
    y++ as empty; // ok
  },

  function(y: bigint) {
    y++ as bigint;
    y-- as bigint;
    ++y as bigint;
    --y as bigint;
  },
];
