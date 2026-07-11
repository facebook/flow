/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {useCallback, useEffect, useState} from 'react';

export hook useToggle(initial: boolean): [boolean, () => void] {
  const [value, setValue] = useState(initial);
  const toggle = useCallback(() => {
    setValue(prev => !prev);
  }, []);
  return [value, toggle];
}

export hook useCounter(
  initial: number,
  step: number = 1,
): {count: number, increment: () => void, decrement: () => void, reset: () => void} {
  const [count, setCount] = useState(initial);
  const increment = useCallback(() => {
    setCount(prev => prev + step);
  }, [step]);
  const decrement = useCallback(() => {
    setCount(prev => prev - step);
  }, [step]);
  const reset = useCallback(() => {
    setCount(initial);
  }, [initial]);
  return {count, increment, decrement, reset};
}

export hook useDebounce(value: string, delay: number): string {
  const [debounced, setDebounced] = useState(value);
  useEffect(() => {
    const timer = setTimeout(() => {
      setDebounced(value);
    }, delay);
    return () => {
      clearTimeout(timer);
    };
  }, [value, delay]);
  return debounced;
}
