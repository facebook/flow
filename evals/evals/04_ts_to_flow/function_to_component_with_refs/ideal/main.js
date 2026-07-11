/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useImperativeHandle, useRef} from 'react';

type InputHandle = {
  focus: () => void,
  clear: () => void,
};

component FancyInput(
  placeholder: string,
  initialValue?: string,
  ref: React.RefSetter<InputHandle>,
) {
  const inputRef = useRef<HTMLInputElement | null>(null);

  useImperativeHandle(ref, () => ({
    focus: () => {
      inputRef.current?.focus();
    },
    clear: () => {
      if (inputRef.current != null) {
        inputRef.current.value = '';
      }
    },
  }));

  return (
    <input
      ref={inputRef}
      placeholder={placeholder}
      defaultValue={initialValue}
    />
  );
}

export default FancyInput;
