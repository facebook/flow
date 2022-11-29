//@flow
declare function test<T>(): ((T => T) => any);

export default test<mixed>();
