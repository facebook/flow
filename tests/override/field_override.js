class Base {
  name: string = "base";
}

class Good extends Base {
  override name: string = "good"; // OK
}

class Bad extends Base {
  override missing: number = 0; // ERROR: `missing` is not declared in `Base`
}

new Good().name as string; // OK
