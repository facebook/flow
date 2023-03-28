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
    <div>
      {flowService.schema.map(item => (
        <div key={item.key}>
          <span>
            <span>{item.key} </span>
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
              <select
                value={flowService.config[item.key] ? 'true' : 'false'}
                onChange={(event: SyntheticInputEvent<>) => {
                  setConfig({
                    ...flowService.config,
                    [item.key]: event.target.value === 'true',
                  });
                }}>
                <option value="true">true</option>
                <option value="false">false</option>
              </select>
            )}
          </span>
        </div>
      ))}
    </div>
  );
}
