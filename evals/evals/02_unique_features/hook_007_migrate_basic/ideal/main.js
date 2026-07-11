/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {useState, useCallback} from 'react';

export hook useBoolean(initial: boolean): {
  value: boolean,
  setTrue: () => void,
  setFalse: () => void,
  toggle: () => void,
} {
  const [value, setValue] = useState(initial);
  const setTrue = useCallback(() => {
    setValue(true);
  }, []);
  const setFalse = useCallback(() => {
    setValue(false);
  }, []);
  const toggle = useCallback(() => {
    setValue(prev => !prev);
  }, []);
  return {value, setTrue, setFalse, toggle};
}

export hook useClampedNumber(
  initial: number,
  min: number,
  max: number,
): [number, (next: number) => void] {
  const [value, setValue] = useState(initial);
  const update = useCallback(
    (next: number) => {
      setValue(Math.min(max, Math.max(min, next)));
    },
    [min, max],
  );
  return [value, update];
}
