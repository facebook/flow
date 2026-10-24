/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

export default function formatAstForDisplay(
  ast: unknown,
  hideLocations: boolean = false,
): unknown {
  function visit(value: unknown, parentKey: ?string): unknown {
    if (Array.isArray(value)) {
      return value.map(child => visit(child, null));
    }
    if (value == null || typeof value !== 'object') {
      return value;
    }
    const entries = Object.entries(value).filter(
      ([key]) =>
        !(parentKey === 'loc' && key === 'source') &&
        !(hideLocations && (key === 'loc' || key === 'range')),
    );
    const typeIndex = entries.findIndex(([key]) => key === 'type');
    if (typeIndex > 0) {
      entries.unshift(...entries.splice(typeIndex, 1));
    }
    return Object.fromEntries(
      entries.map(([key, child]) => [key, visit(child, key)]),
    );
  }
  return visit(ast, null);
}
