// @flow
declare var Immutable: any;
export class State extends (Immutable.Record({placement_count: 0}) as Class<State>) {
}
declare var state: State;
export type T = typeof state.set; // ERROR: expected recursive-definition error
