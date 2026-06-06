/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import {DefinitionType, ScopeType} from '../../src';
import {verifyHasScopes} from '../../__test_utils__/verifyHasScopes';

describe('jsx', () => {
  describe('jsx pragma', () => {
    const code = `
      import React from 'react';
      import CustomReact from 'custom-react';
      import PragmaReact from 'pragma-react';
      <div />
    `;
    const parserOptions = {jsxPragma: 'CustomReact'};
    const pragmaComment = `\x40jsx PragmaReact.foo`;

    describe('Defaults', () => {
      verifyHasScopes(
        `
          /**
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
            ],
          },
        ],
      );
    });
    describe('Explicit Option', () => {
      verifyHasScopes(
        `
          /**
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
            ],
          },
        ],
        parserOptions,
      );
    });
    describe('Comment pragma overrides defaults', () => {
      verifyHasScopes(
        `
          /**
           * ${pragmaComment}
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
            ],
          },
        ],
      );
    });
    describe('Comment pragma overrides explicit option', () => {
      verifyHasScopes(
        `
          /**
           * ${pragmaComment}
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
            ],
          },
        ],
        parserOptions,
      );
    });
  });

  describe('jsx fragment pragma', () => {
    const code = `
      import React from 'react';
      import CustomReact from 'custom-react';
      import {CustomFragment} from 'custom-react';
      import PragmaReact from 'pragma-react';
      import {PragmaFragment} from 'pragma-react';
      <></>
    `;
    const parserOptions = {
      jsxPragma: 'CustomReact',
      jsxFragmentName: 'CustomFragment',
    };
    const pragmaComment = `\x40jsx PragmaReact.foo
      * \x40jsxFrag PragmaFragment.bar
    `;

    describe('Defaults', () => {
      verifyHasScopes(
        `
          /**
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
            ],
          },
        ],
      );
    });
    describe('Explicit Option', () => {
      verifyHasScopes(
        `
          /**
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'CustomFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
            ],
          },
        ],
        parserOptions,
      );
    });
    describe('Comment pragma overrides defaults', () => {
      verifyHasScopes(
        `
          /**
           * ${pragmaComment}
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'PragmaFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
            ],
          },
        ],
      );
    });
    describe('Comment pragma overrides explicit option', () => {
      verifyHasScopes(
        `
          /**
           * ${pragmaComment}
           */
          ${code}
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'CustomFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: false,
              },
              {
                name: 'PragmaReact',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'PragmaFragment',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
            ],
          },
        ],
        parserOptions,
      );
    });
  });

  describe('fbt', () => {
    describe('with option', () => {
      describe('identifier', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbt from 'fbt';
            <fbt />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: false,
                },
                {
                  name: 'fbt',
                  type: DefinitionType.ImportBinding,
                  // fbt is a direct, not indirect reference
                  referenceCount: 1,
                },
              ],
            },
          ],
          {
            fbt: true,
          },
        );
      });
      describe('identifier - fbs', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbs from 'fbs';
            <fbs />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: false,
                },
                {
                  name: 'fbs',
                  type: DefinitionType.ImportBinding,
                  // fbt is a direct, not indirect reference
                  referenceCount: 1,
                },
              ],
            },
          ],
          {
            fbt: true,
          },
        );
      });
      describe('namespace', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbt from 'fbt';
            let foo;
            <fbt:foo />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: false,
                },
                {
                  name: 'fbt',
                  type: DefinitionType.ImportBinding,
                  // fbt is a direct, not indirect reference
                  referenceCount: 1,
                },
                {
                  name: 'foo',
                  type: DefinitionType.Variable,
                  // the namespace name shouldn't reference the variable
                  referenceCount: 0,
                },
              ],
            },
          ],
          {
            fbt: true,
          },
        );
      });
      describe('member expr', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbt from 'fbt';
            <fbt.bar />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: false,
                },
                {
                  name: 'fbt',
                  type: DefinitionType.ImportBinding,
                  // fbt is a direct, not indirect reference
                  referenceCount: 1,
                },
              ],
            },
          ],
          {
            fbt: true,
          },
        );
      });
    });
    describe('without option', () => {
      describe('identifier', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbt from 'fbt';
            <fbt />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: true,
                },
                {
                  name: 'fbt',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                },
              ],
            },
          ],
          {
            fbt: false,
          },
        );
      });
      describe('namespace', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbt from 'fbt';
            <fbt:foo />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: true,
                },
                {
                  name: 'fbt',
                  type: DefinitionType.ImportBinding,
                  // follows the default rules and creates a reference
                  // due to the namespacing
                  // fbt is a direct, not indirect reference
                  referenceCount: 1,
                },
              ],
            },
          ],
          {
            fbt: false,
          },
        );
      });
      describe('member expr', () => {
        verifyHasScopes(
          `
            import React from 'react';
            import fbt from 'fbt';
            <fbt.bar />;
          `,
          [
            {
              type: ScopeType.Module,
              variables: [
                {
                  name: 'React',
                  type: DefinitionType.ImportBinding,
                  referenceCount: 0,
                  eslintUsed: true,
                },
                {
                  name: 'fbt',
                  type: DefinitionType.ImportBinding,
                  // follows the default rules and creates a reference
                  // due to the namespacing
                  // fbt is a direct, not indirect reference
                  referenceCount: 1,
                },
              ],
            },
          ],
          {
            fbt: false,
          },
        );
      });
    });
  });

  describe('Component name references', () => {
    describe('Upper-case references name', () => {
      verifyHasScopes(
        `
          import React from 'react';
          var Component;
          <Component />;
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'Component',
                type: DefinitionType.Variable,
                referenceCount: 1,
              },
            ],
          },
        ],
      );
    });

    describe('Lower-case does not reference name', () => {
      verifyHasScopes(
        `
          import React from 'react';
          var component;
          <component />;
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'component',
                type: DefinitionType.Variable,
                referenceCount: 0,
              },
            ],
          },
        ],
      );
    });

    describe('Opening and closing tag', () => {
      verifyHasScopes(
        `
          import React from 'react';
          var Component;
          <Component></Component>;
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'Component',
                type: DefinitionType.Variable,
                referenceCount: 2,
              },
            ],
          },
        ],
      );
    });

    describe('Member expressions are referenced regardless of casing', () => {
      verifyHasScopes(
        `
          import React from 'react';
          var lowerNamespacedName;
          <lowerNamespacedName.foo />;

          var UpperNamespacedName;
          <UpperNamespacedName.foo />;
        `,
        [
          {
            type: ScopeType.Module,
            variables: [
              {
                name: 'React',
                type: DefinitionType.ImportBinding,
                referenceCount: 0,
                eslintUsed: true,
              },
              {
                name: 'lowerNamespacedName',
                type: DefinitionType.Variable,
                referenceCount: 1,
              },
              {
                name: 'UpperNamespacedName',
                type: DefinitionType.Variable,
                referenceCount: 1,
              },
            ],
          },
        ],
      );
    });
  });

  describe('Opening and closing tag with member expression', () => {
    verifyHasScopes(
      `
          import React from 'react';
          var Namespace;
          <Namespace.Component></Namespace.Component>;
        `,
      [
        {
          type: ScopeType.Module,
          variables: [
            {
              name: 'React',
              type: DefinitionType.ImportBinding,
              referenceCount: 0,
              eslintUsed: true,
            },
            {
              name: 'Namespace',
              type: DefinitionType.Variable,
              referenceCount: 2,
            },
          ],
        },
      ],
    );
  });

  describe('Component generic references', () => {
    verifyHasScopes(
      `
          import React from 'react';
          type A = string;
          function Component<T>(){}
          <Component<A> />;
        `,
      [
        {
          type: ScopeType.Module,
          variables: [
            {
              name: 'React',
              type: DefinitionType.ImportBinding,
              referenceCount: 0,
              eslintUsed: true,
            },
            {
              name: 'A',
              type: DefinitionType.Type,
              referenceCount: 1,
            },
            {
              name: 'Component',
              type: DefinitionType.FunctionName,
              referenceCount: 1,
            },
          ],
        },
        {
          type: ScopeType.Function,
          variables: [
            {
              name: 'arguments',
              type: null,
              referenceCount: 0,
            },
            {
              name: 'T',
              type: DefinitionType.TypeParameter,
              referenceCount: 0,
            },
          ],
        },
      ],
    );
  });

  describe('reference prior to the import should be correctly resolved', () => {
    verifyHasScopes(
      `
jest.mock('Module', () => () => {
  return <div />;
});

import * as React from 'react';
    `,
      [
        {
          type: ScopeType.Module,
          variables: [
            {
              name: 'React',
              type: DefinitionType.ImportBinding,
              referenceCount: 0,
              eslintUsed: true,
            },
          ],
        },
        {
          type: ScopeType.Function,
          variables: [],
        },
        {
          type: ScopeType.Function,
          variables: [],
        },
      ],
    );
  });
});
