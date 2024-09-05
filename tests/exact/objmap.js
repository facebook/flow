type ExactThing = {| a: 1 |};

type Map1To2<T: 1> = 2;
type MappedThing = {[K in keyof ExactThing]: Map1To2<ExactThing[K]>};

// Works as expected
const works: MappedThing = {a: 2};

// Error as expected, `a` needs to be `2`.
const doesError: MappedThing = {a: 3};

// This should error, `b` is not part of the exact object
const shouldntWork: MappedThing = {a: 2, b: 1};
