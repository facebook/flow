// @flow

type Box = {| +type: "GOOD", +value: string |} | {| +type: "BAD", +value: string |};
async function coerce(t: string): Promise<number> {
  let box: Box = { type: "GOOD", value: t };

  let sentinel = { flag: false };
  const p = new Promise<void>(resolve => {
    const intervalId: IntervalID = setInterval(() => {
      if (sentinel.flag) {
        box = { type: "BAD", value: t };
        clearInterval(intervalId);
        resolve();
      }
    }, 1);
  });

  if (box.type === "GOOD") {
    sentinel.flag = true;
    await p;
    if (box.type !== "GOOD") {
      return (box: empty).value; // underconstrained-implicit-instantiation error in LTI due to lack of empty propagation
    }
  }
  throw new Error("Unreachable.");
}
