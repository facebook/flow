// `override` on members of a class expression follows the same rules
// as on class declarations.

class Base {
  greet(): string {
    return "hi";
  }
}

const Sub = class extends Base {
  override greet(): string { // OK: inherited from `Base`
    return "yo";
  }
  override missing(): void {} // ERROR: not declared in `Base`
};

new Sub().greet() as string; // OK
