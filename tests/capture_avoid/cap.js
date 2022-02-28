//@flow


/* This program exhibits variable capture, very subtly.
   When we create the `new P`, we first specialize the `PolyT`
   for `P` by substituting `R` by `this`--the `this` for L.
   We then fix the `ThisClassT` for `P` (which was wrapped in
   the `PolyT` that we just specialized), which involves
   substituting `this` for a self-referential tvar. But when
   we do this substitution, we'll see the `this` that we just
   replaced `R` with as a valid thing to substitute away, even
   though that `this` is supposed to be `L`'s `this`, not `P`'s!
   `L`'s `this` has been captured by the `ThisClassT` abstraction
   for `P`. */
declare class P<+R> {
  constructor(r: R): void;
}

export default class L {
  lef(): P<this> {
    const p = new P(this);
    return p;
  }
}

/* This program, surprisingly, still doesn't
   exhibit free variable capture. Why not?
   After all, we're subsituting a free `X` for
   `Y` within a binding for `X`: why doesn't the
   resulting type, `<X>() => X`, have the return type
   `X` refer to the abstraction?

   The answer is more or less coincidental: we don't
   susbtitute through OpenTs, even fully-resolved ones,
   and the free variable `X` that replaces `Y` is wrapped
   in a fully resolved OpenT. If we made a change that replaced
   the resolved OpenT with the type it's resolved to (a
   change that one would like to be a no-op) we would suddenly
   have capture. */
function f<X>(): X {
  declare var a: <Y>() => <X>() => Y;
  var b = a<X>();
  var c = b<number>();
  return c;
}
