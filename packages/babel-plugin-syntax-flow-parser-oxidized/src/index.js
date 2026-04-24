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

import type {ParserOptions} from 'flow-parser-oxidized';

import * as HermesParser from 'flow-parser-oxidized';

type Options = {
  /**
   * When set to 'flow', will check files for a `@flow` annotation to apply
   * this plugin, otherwise falling back to Babel's parser.
   *
   * This is independent of `parserOpts.flow`, which may customise 'detect'
   * behaviour within hermes-parser (downstream from this plugin).
   *
   * Defaults to 'all'.
   */
  parseLangTypes?: 'flow' | 'all',
};

export default function BabelPluginSyntaxHermesParser(
  // $FlowExpectedError[unclear-type] We don't have types for this.
  api: any,
  options: Options,
): $ReadOnly<{...}> {
  api.assertVersion('^7.0.0 || ^8.0.0-alpha.6');

  const {parseLangTypes = 'all'} = options;

  let curParserOpts: ParserOptions = {};
  let curFilename: ?string = null;

  return {
    name: 'syntax-hermes-parser',

    manipulateOptions(
      opts: $ReadOnly<{parserOpts: ParserOptions, filename?: ?string}>,
    ) {
      curParserOpts = opts.parserOpts;
      curFilename = opts.filename;
    },

    // API suggested via https://babeljs.io/docs/babel-parser#will-the-babel-parser-support-a-plugin-system
    parserOverride(code: string) {
      const filename = curFilename;
      if (
        filename != null &&
        (filename.endsWith('.ts') || filename.endsWith('.tsx'))
      ) {
        return;
      }

      const parserOpts: ParserOptions = {};
      for (const [key, value] of Object.entries(curParserOpts)) {
        if (HermesParser.ParserOptionsKeys.has(key)) {
          // $FlowExpectedError[incompatible-type]
          parserOpts[key] = value;
        }
      }

      if (parseLangTypes === 'flow' && !/@flow/.test(code)) {
        return;
      }

      return HermesParser.parse(code, {...parserOpts, babel: true});
    },

    pre() {
      curParserOpts = {};
    },
  };
}
