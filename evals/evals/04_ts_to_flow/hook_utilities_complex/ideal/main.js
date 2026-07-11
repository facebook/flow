/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {useState, useCallback, useEffect} from 'react';

hook useToggle(initial: boolean = false): [boolean, () => void] {
  const [on, setOn] = useState(initial);
  const toggle = useCallback(() => setOn((v) => !v), []);
  return [on, toggle];
}

hook useList<T>(
  initial: Array<T> = [],
): [Array<T>, (item: T) => void, () => void] {
  const [items, setItems] = useState<Array<T>>(initial);
  const add = useCallback((item: T) => {
    setItems((prev) => [...prev, item]);
  }, []);
  const clear = useCallback(() => {
    setItems([]);
  }, []);
  return [items, add, clear];
}

hook useDebouncedValue<T>(value: T, delayMs: number): T {
  const [debounced, setDebounced] = useState(value);
  useEffect(() => {
    const id = setTimeout(() => setDebounced(value), delayMs);
    return () => clearTimeout(id);
  }, [value, delayMs]);
  return debounced;
}

export {useToggle, useList, useDebouncedValue};
