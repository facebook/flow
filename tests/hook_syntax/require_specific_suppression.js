hook useH<T>(x: T): [T] { return [x];}
useH as (empty) => mixed; // error
// $FlowFixMe[react-rule-hook-incompatible]
useH as (empty) => mixed; // suppressed
// $FlowFixMe
useH as (empty) => mixed; // error, not suppressed
