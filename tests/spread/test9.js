interface I_call {
  (): void;
}
declare var o_call: I_call;
({...o_call}) as {'$call': any}; // error: interfaces cannot be spread

interface I_index {
  [string]: number;
}
declare var o_index: I_index;
({...o_index}) as {'$key': any}; // error: interfaces cannot be spread
({...o_index}) as {'$value': any}; // error: interfaces cannot be spread
