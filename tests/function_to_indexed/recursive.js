//@flow
export type Value =
  | void
  | null
  | boolean
  | number
  | string
  | Aggregate 

export type Aggregate =
  {
    +[string]: Value,
  };

(() => {}: Value);
