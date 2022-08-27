type F = {
  (x: string): number;
  p?: string;
}

function f(x: string) {
  return x.length;
}

(f: F);
