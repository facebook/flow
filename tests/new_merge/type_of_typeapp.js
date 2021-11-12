// @flow

declare class RecordInstance<T> {}

type RecordFactory<Values> = Class<RecordInstance<Values>>;

declare class BaseStore<TState> { getState(): TState; }

declare var State: RecordFactory<{}>;

class Store extends BaseStore<State> {}

declare export default Store;
