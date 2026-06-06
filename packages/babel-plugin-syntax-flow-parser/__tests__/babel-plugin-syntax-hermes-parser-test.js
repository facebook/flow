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

// $FlowExpectedError[untyped-import]
import {transformSync} from '@babel/core';

import hermesParserPlugin from '../src';
import * as HermesParser from 'flow-parser-oxidized';

const MODULE_PREAMBLE = '// @flow\n\n"use strict";\n\n';
const NON_FLOW_MODULE_PREAMBLE = '"use strict";\n\n';

describe('babel-plugin-syntax-hermes-parser', () => {
  const parseSpy = jest.spyOn(HermesParser, 'parse');

  afterEach(() => {
    parseSpy.mockClear();
  });

  test('should parse Flow files', () => {
    const code = MODULE_PREAMBLE + 'const a: number = 1;';
    const output = transformSync(code, {
      plugins: [hermesParserPlugin],
    });
    expect(output.code).toMatchInlineSnapshot(`
      ""use strict";

      const a = 1;"
    `);
    expect(parseSpy).toHaveBeenCalledTimes(1);
  });

  test('should parse files without @flow annotation', () => {
    const code = NON_FLOW_MODULE_PREAMBLE + 'const a: number = 1;';
    const output = transformSync(code, {
      plugins: [hermesParserPlugin],
    });
    expect(output.code).toMatchInlineSnapshot(`
      ""use strict";

      const a = 1;"
    `);
    expect(parseSpy).toHaveBeenCalledTimes(1);
  });

  test('should skip TypeScript files', () => {
    const code = NON_FLOW_MODULE_PREAMBLE + 'const a: number = 1;';
    const output = transformSync(code, {
      plugins: [hermesParserPlugin],
      filename: 'foo.ts',
    });
    expect(output.code).toMatchInlineSnapshot(`
      ""use strict";

      const a = 1;"
    `);
    expect(parseSpy).toHaveBeenCalledTimes(0);
  });

  test('should parse component syntax when enabled', () => {
    const code = MODULE_PREAMBLE + 'component Foo() {}';
    const output = transformSync(code, {
      plugins: [hermesParserPlugin],
      parserOpts: {
        enableExperimentalComponentSyntax: true,
      },
    });
    expect(output.code).toMatchInlineSnapshot(`
      ""use strict";

      function Foo() {}"
    `);
    expect(parseSpy).toHaveBeenCalledTimes(1);
  });

  describe("with parseLangTypes = 'flow'", () => {
    test('should parse Flow files', () => {
      const code = MODULE_PREAMBLE + 'const a: number = 1;';
      const output = transformSync(code, {
        plugins: [hermesParserPlugin],
      });
      expect(output.code).toMatchInlineSnapshot(`
      ""use strict";

      const a = 1;"
    `);
      expect(parseSpy).toHaveBeenCalledTimes(1);
    });

    test('should skip files without @flow annotation ', () => {
      const code = NON_FLOW_MODULE_PREAMBLE + 'class Foo {}';
      const output = transformSync(code, {
        plugins: [[hermesParserPlugin, {parseLangTypes: 'flow'}]],
      });
      expect(output.code).toBe(code);
      expect(parseSpy).toHaveBeenCalledTimes(0);
    });
  });
});
