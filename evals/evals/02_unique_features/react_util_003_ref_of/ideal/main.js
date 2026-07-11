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

component TextInput(value: string, ref: React.RefSetter<HTMLInputElement>) {
  return <input value={value} readOnly={true} ref={ref} />;
}

component LabeledField(
  label: string,
  value: string,
  ref: React.RefSetter<React.RefOf<TextInput>>,
) {
  return (
    <label className="field">
      <span className="field-label">{label}</span>
      <TextInput value={value} ref={ref} />
    </label>
  );
}

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
