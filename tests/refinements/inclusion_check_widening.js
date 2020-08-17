// @flow

// Flow checks that, in conditional expressions, the refining (sentinel) value is
// a subtype of the type of the value that is being refined. This check is not
// necessary for soundness, but it helps prevent some common bugs. These checks
// are also not necessary for propagating true value flow.
//
// A potential pitfall when checking for these violations, is to have these checks
// affect the result of inference, by flowing the "sentinel" type to the refined
// type. This may cause these types to flow downstream, far from the conditional
// cite, causing spurious errors.
//
// The tests in the file exercise scenarios, where having these checks as part of
// inference produces surprising results.


// Example 1

declare function useState<S>(state: (() => S) | S): [S, ((S => S) | S) => void];
const [value, setter] = useState(null);
declare var Enum: {A: 'A', B: 'B'};

// The conditional below may be seen as a refinement on `Enum`. In that case, we
// make sure that `value` is a valid type for `Enum.A` (see MatchingPropT).
// If we let this check affect inference, then this will affect the type of `value`,
// as well as `setter`. The latter causes the call below to fail.
if (value === Enum.A) {}
setter('B');  // TODO okay


// Example 2

const x = null;
const y = x && x.f.g;
// `null` may reach `y` here, and so this should be reported as an error ('val'
// cannot refine `null`). It does not, because we allow the check of 'val' <: TypeOf(y)
// to expand the type of `y`.
if (y === 'val') {} // TODO error


// Example 3

type V<T: {...}> = { key2: $Keys<T> };
declare var column: V<{...}>;
const {key2} = column;
if (key2 === 'a') {} // TODO error (key2 does not include 'a')


// Example 4

class C {}
type State = { prop: ?C };

class D {
  state: State = { prop: null };
  m(): void {
    const {prop} = this.state;
    if (
        prop !== undefined &&
        prop !== null &&
        prop !== 0 // TODO error (0 incompatible with C)
       ) {}
  }
}


// Example 5

const {key5} = {key5: 'a'};
const _ = key5 === 'b' ? null : null;
(key5: 'a'); // TODO okay
