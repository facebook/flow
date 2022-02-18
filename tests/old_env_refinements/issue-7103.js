// @flow

declare class MyPromise {
  constructor(_: mixed): void;
}

type Box = {| +type: "GOOD", +value: string |} | {| +type: "BAD", +value: string |};
function coerce(t: string): number {
  let box: Box = { type: "GOOD", value: t };
  if (box.type === "GOOD") {
    new MyPromise((resolve, reject) => {
      box = { type: "BAD", value: t };
    });
    if (box.type !== "GOOD") {
      return (box: empty).value;
    }
  }
  throw new Error("Unreachable.");
}
