const simple = value === 1 ? "one" : Number.isNaN(value) ? "nan" : "other";
const bound = ($$gen$m0 => {
  if (Array.isArray($$gen$m0) && $$gen$m0.length >= 1) {
    const head = $$gen$m0[0];
    const tail = $$gen$m0.slice(1);
    return head;
  }
  return null;
})(getValue());
const empty = ($$gen$m1 => {
  throw (
    Error(
      "Match: No case succesfully matched. Make exhaustive or add a wildcard case using '_'. Argument: " + $$gen$m1,
    )
  );
})(getValue());
$$gen$m2: {
  const $$gen$m3 = value;
  if (
    (typeof $$gen$m3 === "object" && $$gen$m3 !== null ||
      typeof $$gen$m3 === "function") &&
      "a" in $$gen$m3 &&
      $$gen$m3.b === 0
  ) {
    const a = $$gen$m3.a;
    break $$gen$m2;
  }
  {
    break $$gen$m2;
  }
}
