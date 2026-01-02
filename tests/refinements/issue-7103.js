declare class MyPromise {
  constructor(_: unknown): void;
}

type Box = {| +type: "GOOD", +value: string |} | {| +type: "BAD", +value: string |};
function coerce(t: string): number {
  let box: Box = { type: "GOOD", value: t };
  if (box.type === "GOOD") {
    new MyPromise((resolve: unknown, reject: unknown) => {
      box = { type: "BAD", value: t };
    });
    if (box.type !== "GOOD") {
      return (box: empty).value;
    }
  }
  throw new Error("Unreachable.");
}
