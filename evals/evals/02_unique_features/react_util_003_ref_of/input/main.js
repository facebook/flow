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

// TODO: Implement the `TextInput` and `LabeledField` components used below.

export default component App() {
  const fieldRef = useRef<HTMLInputElement | null>(null);
  return (
    <div>
      <LabeledField label="Name" value="Ada" ref={fieldRef} />
      <button type="button" onClick={() => fieldRef.current?.focus()}>
        Focus name
      </button>
    </div>
  );
}
