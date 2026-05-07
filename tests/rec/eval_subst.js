type State = {pointers: Array<{ id?: number, ... }>};

declare const state: State;

const partialState = { pointers: state.pointers };
partialState.pointers = partialState.pointers.filter(Boolean);
partialState as State;
