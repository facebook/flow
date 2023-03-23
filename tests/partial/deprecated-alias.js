({}: $Partial<{x: string, y: string}>); // ERROR: deprecated utility - use `Partial` instead

function shadowed() {
  type $Partial = number; // OK
  const x: $Partial = 1; // OK
}
