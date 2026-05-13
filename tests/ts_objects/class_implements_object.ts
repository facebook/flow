// Class `implements` an object-typed upper -- e.g. a utility-type
// instantiation (Omit/Pick), a user-defined generic that bottoms out at an
// object type, or a comma-separated list of object-typed clauses. In .js
// Flow rejects these with `[cannot-implement]`. In .ts we accept them and
// run the same structural check as for an InterfaceKind upper.

type Animal = {
  name: string;
  age: number;
  sound: string;
};

// (1) implements Omit<...>
class Pet implements Omit<Animal, "sound"> {
  name: string = "rex";
  age: number = 3;
}

// (2) implements Pick<...>
class PetSummary implements Pick<Animal, "name" | "age"> {
  name: string = "rex";
  age: number = 3;
  thumbnail: string = "rex.png";
}

// (3) implements a user-defined generic that bottoms out at an ObjT
type Box<T> = {value: T};
class StringBox implements Box<string> {
  value: string = "hi";
}

// (4) implements multiple object-typed clauses (comma-separated -- each
// clause is independently dispatched, so the new ObjT arm catches both)
type HasId = {id: string};
type HasName = {name: string};
class Tagged implements HasId, HasName {
  id: string = "1";
  name: string = "tagged";
}

// (5) Negative: a missing required prop on the implementing class still
// errors -- the new arm runs `structural_subtype`, which catches the gap.
class Bad implements Omit<Animal, "sound"> {
  name: string = "rex";
  // ERROR: `age` is missing
}

// (6) Negative: a wrong-typed prop on the implementing class still errors.
class WrongType implements Box<string> {
  value: number = 1; // ERROR: number incompatible with string
}

// (7) Regression: implementing a real interface still works (existing
// InterfaceKind arm).
interface IPet {
  name: string;
}
class RealPet implements IPet {
  name: string = "rex";
}

// (8) Negative: the new arm intentionally excludes Indexed object uppers,
// because `inst_structural_subtype` only enforces the upper indexer when
// the implementing class carries `inst_dict`. Until that hole is plugged,
// `implements Record<...>` must still fall through to the existing error.
class NoIndexer implements Record<string, number> { // ERROR: indexed ObjT not accepted
}
