/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {useState, useCallback} from 'react';

declare const __DEV__: boolean;

hook useLiveValue(initial: number): [number, () => void] {
  const [value, setValue] = useState(initial);
  const refresh = useCallback(() => {
    setValue(prev => prev + 1);
  }, []);
  return [value, refresh];
}

hook useFrozenValue(initial: number): [number, () => void] {
  return [initial, () => {}];
}

const useValue: hook (initial: number) => [number, () => void] = __DEV__
  ? useLiveValue
  : useFrozenValue;

export default useValue;
