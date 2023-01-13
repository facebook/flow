type M = {
  a: number,
  b: string,
}

declare var v: M['a' | 'b'];
(v: string); // error
(v: number); // error
(v: number | string); // ok

v = 3;
v = "";
v = true; // error

declare var union: number | string;
(union: M['a' | 'b']);

type W = M['a' | 'b'];
declare var w: W;
(w: W);
(w: M['a']); // error


/* Termination - the following code produces a recursive union type of the form
 *
 * type r = r | Array<t>
 *
 * To avoid termination issues here, we use ConstFoldExpansion.guard. */

declare export function useState<S>(
  initialState: (() => S) | S,
): [S, ((S => S) | S) => void];
declare export function useRef<T>(initialValue: T): {|current: T|};
declare var NUM: number;
const [state, setState] = useState(1);
useRef(([]: Array<number>)).current[state];
setState(st => st === NUM ? st : st);


/* Regressions due to interaction with unions */

type Key = 'a' | 'b';
type T = {
  a: Array<Key>,
  b: Array<Key>
};
(['a']: T[Key]); // Expected [speculation-ambiguous] error
