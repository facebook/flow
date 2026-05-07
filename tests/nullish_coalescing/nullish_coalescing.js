function foo(a: ?string, b: ?number, c : boolean) {
  (undefined ?? "hello") as string;
  (a ?? "hello") as string;
  (a ?? "hello") as empty; // Error
  (a ?? 42) as string | number;
  (a ?? 42) as empty; // Error
  var d = null;
  (d ?? "true") as string; // constant-condition error
  (d ?? a ?? b) as ?(string | number); // constant-condition error
  if (c) {
    d = a;
  } else {
    d = b;
  }
  (d ?? "hello") as string | number;
  (d ?? "hello") as empty; // Error
}

type Bar<T = unknown> = ?boolean;
function bar(value: Bar<>) {
  const x: boolean = value ?? true;
  const y: boolean = value as ?boolean ?? true;
}

function refinement(x: unknown): unknown {
  const result = x ?? 0;
  if (result == null) {
    result as empty;
  }
  return result;
}

function g(x: any): string {
  return x ?? 1;
}

function h(x: {}): {} {
  return x ?? 1;
}

function i(x: {}): {} {
  return x ?? 1;
}
