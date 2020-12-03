//@flow
export type Covariant<+T> = T;
export type Contravariant<-T> = T => void;
export type Invariant<T> = T;
