/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {createEditor} from './js/tryFlow';

createEditor(
  window.tryFlowDefaultVersion,
  document.getElementById("code"),
  document.getElementById("results"),
  window.tryFlowVersions
);
