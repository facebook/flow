// @flow

declare function useCallback<T: (...args: $ReadOnlyArray<empty>) => mixed>(cb: T): T;

const f = useCallback((foo) => {}); // ANNOT
f(3);
