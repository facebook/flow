const f = () => {
  (this: number); // ERROR
};
f.a = 1;

(f.a: number); // OK
(f.a: empty); // ERROR

const g = () => {};
g.v = true;

(g.v: boolean); // OK

f.c = ""; // OK
(f.c: string); // OK

g.w = 1; // OK
(g.w: number); // OK

declare function debounce<Args>(
  func: (...Args) => mixed,
): (...Args) => void;
{
  const d = debounce((s: string, n: number): void => {});
  const f = (s: string) => (n: number) => {
    d(s, n); // OK
    d(n, s); // ERROR
  };
}

// Annotated
{
  const f: {(): number, a: string} = () => 1;
  f.a = "hi"; // OK
  f.a = 1; // ERROR
  f.xxx; // ERROR
}
