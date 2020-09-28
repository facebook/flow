function foo(a: ?string, b: ?number, c : bool) {
  (undefined ?? "hello" : string);
  (a ?? "hello" : string);
  (a ?? "hello" : empty); // Error
  (a ?? 42 : string | number);
  (a ?? 42 : empty); // Error
  var d = null;
  (d ?? "true" : string);
  (d ?? a ?? b : ?(string | number));
  if (c) {
    d = a;
  } else {
    d = b;
  }
  (d ?? "hello" : string | number);
  (d ?? "hello" : empty); // Error
}

type Bar<T = mixed> = ?boolean;
function bar(value: Bar<>) {
  const x: boolean = value ?? true;
  const y: boolean = (value: ?boolean) ?? true;
}

function refinement(x: mixed): mixed {
  const result = x ?? 0;
  if (result == null) {
    (result: empty);
  }
  return result;
}

function g(x: any): string {
  return x ?? 1;
}
