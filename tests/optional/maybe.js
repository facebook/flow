function foo(x?: string): ?string {
    return x;
}

function bar(x: ?{bar: number}) {
  x['bar']; // Error
}
