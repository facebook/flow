declare var m: $Required<{
  get a(): number,
  get b(): number,
  set c(v: number): void
}>;

(m.a: number);
m.a = 1; // error

(m.a: number);
m.b = 1; // error

m.c; // error
m.c = 1;

declare class B {
  get a(): number;
}

declare var bar: $Required<B>;

(bar.a: number); // no error
