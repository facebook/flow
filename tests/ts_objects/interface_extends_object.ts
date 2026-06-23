// Interface extending an object-typed upper -- e.g. a utility-type
// instantiation (Omit/Pick/Record), a user-defined generic that bottoms out
// at an object type, or an intersection of object types. In .js Flow rejects
// these with `is not inheritable`. In .ts we accept them and run the same
// override-consistency check as for an InstanceT super.

type Animal = {
  name: string;
  age: number;
  sound: string;
};

// (1) extends Omit<...>
interface Pet extends Omit<Animal, "sound"> {
  owner: string;
}
declare const pet: Pet;
pet.name satisfies string; // OK
pet.age satisfies number; // OK
pet.owner satisfies string; // OK

// (2) extends Pick<...>
interface PetSummary extends Pick<Animal, "name" | "age"> {
  thumbnail: string;
}
declare const ps: PetSummary;
ps.name satisfies string; // OK
ps.age satisfies number; // OK
ps.thumbnail satisfies string; // OK

// (3) extends a user-defined generic that bottoms out at an ObjT
type Box<T> = {value: T};
interface StringBox extends Box<string> {
  label: string;
}
declare const sb: StringBox;
sb.value satisfies string; // OK
sb.label satisfies string; // OK

// (4) extends Record<K, V> -- the indexer is reachable through the super chain
interface Bag extends Record<string, number> {}
declare const bag: Bag;
bag["any-key"] satisfies number; // OK

// (5) extends an intersection of object types (free via existing IntersectionT
// fanout at flow_js.ml:1860 -- each branch routes into the new ObjT arm)
type HTMLAttrs = {id: string; className?: string};
type DataAttrs = {"data-id"?: string};
interface DivProps extends Omit<HTMLAttrs, "className">, DataAttrs {
  extra: number;
}
declare const dp: DivProps;
dp.id satisfies string; // OK
dp.extra satisfies number; // OK

// (6) HTMLProps-style: Omit composed with another generic
type Attrs<T> = {tag: T; id: string};
type HTMLProps<T> = Omit<Attrs<T>, "tag">;
interface ComponentProps extends HTMLProps<"div"> {
  className: string;
}
declare const cp: ComponentProps;
cp.id satisfies string; // OK
cp.className satisfies string; // OK

// (7) Regression: interface extends interface (existing InstanceT arm) still works.
interface BaseI {
  base: number;
}
interface ExtI extends BaseI {
  extra: string;
}
declare const ei: ExtI;
ei.base satisfies number; // OK
ei.extra satisfies string; // OK

// (8) Regression: bad-override into an ObjT super still errors. The new arm
// runs the same `check_super` helper, so prop-type mismatches are caught.
interface BadOverride extends Omit<Animal, "sound"> {
  name: number; // ERROR: number incompatible with string
}

// (9) Pin: type parameters are not in scope in an interface `extends`
// clause -- name resolution rejects `T` before the new ObjT arm could
// fire. This holds even with a constraint (e.g. `<T extends Foo>`).
interface Wrap<T> extends T {} // ERROR: cannot-resolve-name on T
