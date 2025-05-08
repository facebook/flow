function test0() {
    declare function outer(...Array<(x: number) => void>): void
    declare function inner<A>(iter: Array<A>): Array<A>;

    outer(...[(x) => { // TODO okay no annotation required
        x as string; // TODO error number ~> string
    }]);

    outer(...inner([(x) => { // TODO okay no annotation required
        x as string; // TODO error number ~> string
    }]));
}

// Regression tests from support post
function test1() {
    declare var arrayBuffer: ArrayBuffer;
    String.fromCharCode(...Array.from(new Uint8Array(arrayBuffer))); // okay - correct overload of Array.from selected
}

function test2() {
    declare function foo(
      fn: (
          ...args: Parameters<() => Set<(x: string) => void>>
      ) => ReturnType<() => Set<(x: string) => void>>,
    ): void;

    foo(() => new Set([
      x => {
        x as number; // error string ~> number
      }
    ]));
}
