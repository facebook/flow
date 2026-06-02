declare abstract class D {
  abstract m(): string;
  concrete(): void;
}

((d: D) => {
  d.m() as string; // OK
});

new D(); // ERROR

class E extends D { // ERROR: E missing abstract `m`
  concrete(): void {}
}

class F extends D {
  m(): string {
    return "ok";
  }
  concrete(): void {}
}
new F().m() as string; // OK

declare class G {
  abstract m(): void; // ERROR: abstract member in non-abstract class
}
