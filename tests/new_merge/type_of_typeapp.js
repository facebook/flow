// @flow

declare class RecordInstance<T> {}

type RecordFactory<T> = Class<RecordInstance<T>>;

declare class BaseStore<TState> { getState(): TState; }

declare const State: RecordFactory<{...}>;

class Store extends BaseStore<State> {}

declare export default Store;
