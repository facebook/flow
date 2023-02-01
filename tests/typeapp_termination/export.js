export interface PromiseLike<T> {
  then<TResult1 = T, TResult2 = empty>(
    onfulfilled?: (value: T) => TResult1 | PromiseLike<TResult1>
  ): PromiseLike<TResult1 | TResult2>;
}
