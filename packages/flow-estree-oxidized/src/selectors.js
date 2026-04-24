/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {ESQueryNodeSelectorsWithoutFallback} from './generated/HermesESTreeSelectorTypes';

export type ESQueryNodeSelectors = {
  ...ESQueryNodeSelectorsWithoutFallback,

  // We want to allow consumers to manually type their weird selectors.
  // If we use the \`ESNode\` type here then flow will error on cases like this:
  // 'FunctionDeclaration[id="foo"]'(node: FunctionDeclaration) {...}
  // But this sucks as it means someone would then have to manually do an \`if\`
  // check inside the selector body.
  +[selector: string]: (node: $FlowFixMe) => void,
};

export type {ESQueryNodeSelectorsWithoutFallback} from './generated/HermesESTreeSelectorTypes';

export {};
