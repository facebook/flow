/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import {testCodemod} from '../../../testUtils/codemodTestUtils';
import codemod from '../eliminateAbstractComponent';

testCodemod('eliminateAbstractComponent', codemod, {
  transformed: [
    {
      description: 'strategy 1',
      code: `type A = React.AbstractComponent<Props>;
             type B = React.AbstractComponent<Props, mixed>;`,
      output: `type A = React.ComponentType<Props>;\ntype B = React.ComponentType<Props>;`,
    },
    {
      description: 'strategy 2',
      code: `type A = React.ElementConfig<React.AbstractComponent<Props>>;
             type B = React.ElementConfig<React.AbstractComponent<Props, Instance>>;`,
      output: `type A = Props;\ntype B = Props;`,
    },
    {
      description: 'strategy 3',
      code: `type A = React.ElementRef<React.AbstractComponent<Props, Instance>>;
             type B = React.ElementRef<React.AbstractComponent<Props, Instance, Renders>>;`,
      output: `type A = Instance;\ntype B = Instance;`,
    },
    {
      description: 'strategy 4',
      code: `type A = React.ElementRef<React.AbstractComponent<Props>>;`,
      output: `type A = mixed;`,
    },
  ],
  ignored: [
    {
      description: 'strategy 1 fail',
      code: 'type A = React.AbstractComponent<Props, Instance>;',
    },
    {
      description: 'strategy 2 fail',
      code: 'type A = React.ElementConfig<1>;',
    },
  ],
});
