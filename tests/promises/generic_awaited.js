function allGeneric<T>(promises: Array<Promise<T>>): Promise<Array<T>> {
  return Promise.all(promises);
}

function anyGeneric<T>(promises: Array<Promise<T>>): Promise<T> {
  return Promise.any(promises);
}

function allSettledGeneric<T>(
  promises: Array<Promise<T>>,
): Promise<Array<$SettledPromiseResult<T>>> {
  return Promise.allSettled(promises);
}

declare const awaited: Awaited<PromiseLike<number>>;
awaited as PromiseLike<number>;
awaited as number; // ERROR
