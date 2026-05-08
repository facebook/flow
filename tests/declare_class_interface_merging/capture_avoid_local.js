interface CaptureLocal<A, B> {
  same: (value: B) => A;
}
interface CaptureLocal<B, A> {
  local: <A>(x: A) => B;
}

declare const local: CaptureLocal<number, string>;
local.local(true) as number;
local.local(true) as string; // ERROR

interface CaptureLocalConflict<A, B> {
  same: <T>(x: T) => A;
}
interface CaptureLocalConflict<B, A> {
  same: <A>(x: A) => B;
}

declare const conflict: CaptureLocalConflict<number, string>;
conflict.same(true) as number;
conflict.same(true) as string; // ERROR
