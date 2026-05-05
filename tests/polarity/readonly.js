type Valid1<+T> = Readonly<{foo: T}>; // ok
type Valid2<-T> = Readonly<{foo: (T) => void}>; // ok
type Invalid1<+T> = Readonly<{foo: (T) => void}>; // error
type Invalid2<-T> = Readonly<{foo: T}>; // error

type TriggersEval<+T extends {...}> = Readonly<{...T}>; // no error: the generic EvalT is stuck
