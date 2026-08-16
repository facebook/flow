// insert-type declines to annotate a value whose type has a `unique symbol`
// key. There is no annotation spelling for such a key, and dropping it would
// describe a different type -- one the value no longer satisfies, since an
// exact object rejects the extra key. So `o` is left unannotated below.
declare const s: unique symbol;
const o = {[s]: 1, foo: 2};

// A neighbouring object with only string keys is still annotated, so the
// refusal above is specific to the symbol key.
const plain = {foo: 2};
