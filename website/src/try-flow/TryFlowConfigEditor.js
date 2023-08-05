/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import type FlowJsServices from './flow-services';
import styles from './TryFlow.module.css';

type Props = $ReadOnly<{
  flowService: ?FlowJsServices,
  setConfig: ({[string]: mixed}) => void,
}>;

export default function TryFlowConfigEditor({
  flowService,
  setConfig,
}: Props): React.Node {
  if (flowService == null) {
    return 'Loading Flow configuration schema';
  }
  if (flowService?.schema == null) {
    return 'Configuration is not supported on this version of Flow.';
  }
  return (
    <table>
      {flowService.schema.map(item => (
        <tr key={item.key}>
          <td className={styles.tryEditorConfigInputCell}>
            {item.type === 'enum' ? (
              <select
                value={flowService.config[item.key]}
                onChange={(event: SyntheticInputEvent<>) => {
                  setConfig({
                    ...flowService.config,
                    [item.key]: event.target.value,
                  });
                }}>
                {item.choices.map(choice => (
                  <option key={choice} value={choice}>
                    {choice}
                  </option>
                ))}
              </select>
            ) : (
              <input
                type="checkbox"
                id={item.key}
                checked={flowService.config[item.key]}
                onChange={(event: SyntheticInputEvent<>) => {
                  setConfig({
                    ...flowService.config,
                    [item.key]: event.target.checked,
                  });
                }}
              />
            )}
          </td>
          <td className={styles.tryEditorConfigLabelCell}>
            <label for={item.key}>{item.key}</label>
          </td>
        </tr>
      ))}
    </table>
  );
}
