declare function maybeString(): ?string;

function test() {
  const key = maybeString();
  if (key == null) {
    return;
  }
  const obj = { [key]: "" }; // okay: union boils away with concretization
}
