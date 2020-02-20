// @flow

type State = {| pointers: Array<{ id?: number }> |};

declare var state: State;

const partialState = { pointers: state.pointers };
partialState.pointers = partialState.pointers.filter(Boolean);
(partialState: State);
