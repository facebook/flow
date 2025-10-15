type Valid1<+T> = $ReadOnly<{foo: T}>; // ok
type Valid2<-T> = $ReadOnly<{foo: (T) => void}>; // ok
type Invalid1<+T> = $ReadOnly<{foo: (T) => void}>; // error
type Invalid2<-T> = $ReadOnly<{foo: T}>; // error

type TriggersEval<+T: {...}> = $ReadOnly<{...T}>; // no error: the generic EvalT is stuck
