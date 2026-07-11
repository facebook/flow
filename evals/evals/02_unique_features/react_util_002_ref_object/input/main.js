/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useRef} from 'react';

// TODO: Implement the `selectAll` helper used below.

export default component SearchField(initialQuery: string) {
  const inputRef = useRef<HTMLInputElement | null>(null);
  return (
    <div>
      <input ref={inputRef} defaultValue={initialQuery} />
      <button type="button" onClick={() => selectAll(inputRef)}>
        Select all
      </button>
    </div>
  );
}
