// @flow
declare var Immutable: any;
const StateRecord: Class<State> = Immutable.Record({});
class State extends StateRecord {
}
declare var state: State;
export type T = typeof state.set; // ERROR: expected recursive-definition error
function f(s: State): State {
  return s.set('x', 1);
}
