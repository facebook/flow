/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

/*
 * We should explicitly list all the labels below, so that src/js/docs-categories.js that generates
 * the /en/docs page will automatically work.
 *
 * For categories, the first item must be the index page.
 */
module.exports = {
  docsSidebar: [
    {
      type: 'category',
      label: 'Introduction',
      items: ['getting-started', 'install', 'usage'],
    },
    {
      type: 'doc',
      id: 'faq',
      label: 'FAQ',
    },
    {
      type: 'category',
      label: 'Type Annotations',
      items: [
        'types/index',
        'types/primitives',
        'types/literals',
        'types/mixed',
        'types/empty',
        'types/any',
        'types/maybe',
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
        'types/conditional',
        'types/mapped-types',
        'types/type-guards',
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
        'lang/types-and-expressions',
        'lang/variables',
        'lang/subtypes',
        'lang/type-hierarchy',
        'lang/variance',
        'lang/nominal-structural',
        'lang/depth-subtyping',
        'lang/width-subtyping',
        'lang/refinements',
        'lang/lazy-modes',
        'lang/types-first',
        'lang/annotation-requirement',
      ],
    },
    {
      type: 'category',
      label: 'React',
      items: [
        'react/index',
        'react/component-syntax',
        'react/function-and-class-components',
        'react/events',
        'react/refs',
        'react/hoc',
        'react/render-types',
        'react/types',
        process.env.INTERNAL_STATIC_DOCS && 'react/multiplatform',
      ].filter(Boolean),
    },
    {
      type: 'category',
      label: 'Flow Enums',
      items: [
        'enums/index',
        'enums/enabling-enums',
        'enums/defining-enums',
        'enums/using-enums',
        'enums/migrating-legacy-patterns',
      ],
    },
    {
      type: 'doc',
      id: 'declarations/index',
      label: 'Declarations',
    },
    {
      type: 'category',
      label: 'Library Definitions',
      items: ['libdefs/index', 'libdefs/creation'],
    },
    {
      type: 'doc',
      id: 'errors/index',
      label: 'Error Suppressions',
    },
    {
      type: 'category',
      label: 'Linting',
      items: [
        'linting/index',
        'linting/flowlint-comments',
        'linting/rule-reference',
      ],
    },
    {
      type: 'doc',
      id: 'strict/index',
      label: 'Flow Strict',
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
        'config/version',
        'config/options',
        'config/include',
        'config/ignore',
        'config/untyped',
        'config/declarations',
        'config/libs',
        'config/lints',
      ],
    },
    {
      type: 'category',
      label: 'Tools',
      items: ['tools/babel', 'tools/eslint', 'tools/flow-remove-types'],
    },
    {
      type: 'category',
      label: 'Editors',
      items: [
        'editors/index',
        'editors/vscode',
        'editors/sublime-text',
        'editors/vim',
        'editors/emacs',
        'editors/webstorm',
      ],
    },
  ],
};
