interface Capture<A, B> {
  same: (value: B) => A;
}
interface Capture<B, A> {
  weird: <A>(x: A) => B;
}

declare export const capt: Capture<number, string>;
