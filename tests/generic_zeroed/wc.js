function coerce<T, U>(t: T): U {
  type Fruit<T> =
    | { readonly type: "APPLE", readonly value: T }
    | { readonly type: "BAD_APPLE", readonly value: empty };
  function corrupt<S extends string>(s: S): S {
    return "BAD_" + s;
  }
  const fruit: Fruit<T> = { type: corrupt("APPLE") as "APPLE", value: t };
  if (fruit.type === "BAD_APPLE") {
    return fruit.value;
  } else {
    throw new Error("Unreachable.");
  }
}
const twelve: number = coerce("twelve"); // no type error!
twelve.toFixed(); // runtime error!
