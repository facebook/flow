interface I_call {
  (): void;
}
declare var o_call: I_call;
({ ...o_call }: { "$call": any }); // error: callable should not be copied

interface I_index {
  [string]: number;
}
declare var o_index: I_index;
({ ...o_index }: { "$key": any }); // error: indexer key should not be copied
({ ...o_index }: { "$value": any }); // error: indexer value should not be copied
