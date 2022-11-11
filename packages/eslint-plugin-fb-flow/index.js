/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @oncall flow
 */

'use strict';

module.exports = {
  rules: {
    'flow-enums-default-if-possible': require('./rules/flow-enums-default-if-possible'),
    'no-flow-enums-object-mapping': require('./rules/no-flow-enums-object-mapping'),
    'use-exact-by-default-object-type': require('./rules/use-exact-by-default-object-type'),
    'use-flow-enums': require('./rules/use-flow-enums'),
    'use-indexed-access-type': require('./rules/use-indexed-access-type'),
  },
};
