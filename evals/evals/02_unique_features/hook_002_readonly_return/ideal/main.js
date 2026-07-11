/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {useCallback, useState} from 'react';

export default hook useMap<K, V>(initial?: ReadonlyArray<[K, V]>): {
  get: (key: K) => V | void,
  set: (key: K, value: V) => void,
  delete: (key: K) => void,
  has: (key: K) => boolean,
  size: number,
  entries: ReadonlyArray<[K, V]>,
} {
  const [map, setMap] = useState<Map<K, V>>(
    () => new Map(initial ?? []),
  );

  const get = useCallback((key: K): V | void => map.get(key), [map]);

  const set = useCallback(
    (key: K, value: V): void => {
      setMap((prev) => {
        const next = new Map(prev);
        next.set(key, value);
        return next;
      });
    },
    [],
  );

  const del = useCallback(
    (key: K): void => {
      setMap((prev) => {
        const next = new Map(prev);
        next.delete(key);
        return next;
      });
    },
    [],
  );

  const has = useCallback((key: K): boolean => map.has(key), [map]);

  return {
    get,
    set,
    delete: del,
    has,
    size: map.size,
    entries: Array.from(map.entries()),
  };
}

