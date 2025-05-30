type Box = {| +type: "GOOD", +value: string |} | {| +type: "BAD", +value: string |};
type Result = {| +type: "PENDING" |} | {| +type: "DONE", value: number |};
function coerce(t: string): number {
  let box: Box = { type: "GOOD", value: t };
  function* broken(): Iterator<Result> {
    if (box.type === "GOOD") {
      yield { type: "PENDING" }; // TODO(T225770374)
      if (box.type !== "GOOD") {
        yield { type: "DONE", value: (box: empty).value }; // TODO(T225770374)
      }
    }
  }
  const it = broken();
  it.next(); // consume
  box = { type: "BAD", value: t };
  const result = it.next();
  if (result.done || result.value.type !== "DONE")
    throw new Error("Unreachable.");
  return result.value.value;
}
