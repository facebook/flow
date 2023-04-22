type T = $Shape<{a: foo}>; // ERROR

function shadowed() {
  type $Shape = number; // OK
  const x: $Shape = 1; // OK
}
