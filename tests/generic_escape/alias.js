//@flow

declare class Pr<+T> {}

type Tr = <Return>(() => Pr<Return>) => Return;

declare function aw<T>(p: Pr<T> | T): T;

(f => {
  return aw(f());
}: Tr);
