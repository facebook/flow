/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

module.exports = {
  someSidebar: [
    {
      type: 'category',
      label: 'Introduction',
      items: ['getting-started', 'install', 'usage'],
    },
    'faq',
    {
      type: 'category',
      label: 'Type Annotations',
      items: [
        'types/index',
        'types/primitives',
        'types/literals',
        'types/mixed',
        'types/any',
        'types/maybe',
        'types/variables',
        'types/functions',
        'types/objects',
        'types/arrays',
        'types/tuples',
        'types/classes',
        'types/aliases',
        'types/opaque-types',
        'types/interfaces',
        'types/generics',
        'types/unions',
        'types/intersections',
        'types/indexed-access',
        'types/typeof',
        'types/casting',
        'types/utilities',
        'types/modules',
        'types/comments',
      ],
    },
    {
      type: 'category',
      label: 'Type System',
      items: [
        'lang/index',
        'lang/types-and-expressions',
        'lang/subtypes',
        'lang/variance',
        'lang/nominal-structural',
        'lang/depth-subtyping',
        'lang/width-subtyping',
        'lang/refinements',
        'lang/lazy-modes',
        'lang/types-first',
      ],
    },
    {
      type: 'category',
      label: 'Flow CLI',
      items: ['cli/index', 'cli/coverage', 'cli/annotate-exports'],
    },
    {
      type: 'category',
      label: 'Configuration',
      items: [
        'config/index',
        'config/include',
        'config/ignore',
        'config/untyped',
        'config/libs',
        'config/lints',
        'config/options',
        'config/version',
        'config/declarations',
      ],
    },
  ],
};
