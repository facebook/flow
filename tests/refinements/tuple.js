// @flow

function length_refinement(a: [string] | [number, string]): void {
  if (a.length === 1) {
    (a[0]: string);
  } else {
    (a[0]: number);
    (a[1]: string);
  }
}

function sentinel_refinement(k: ["a", number] | ["b", string]): void {
  if (k[0] === "a") {
    (k[1]: number);
  } else {
    (k[1]: string);
  }
}

function sentinel_numeric_equiv(k: [number, "a"] | [string, "b"]): void {
  if (k[1.00] === "a") {
    (k[0]: number);
  } else {
    (k[0]: string);
  }
  if (k[0.999999999999999999999999999999999999999999999] === "a") {
    (k[0]: number);
  } else {
    (k[0]: string);
  }
}
