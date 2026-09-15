/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../../.prettierrc.json';
import {translateFlowDefToTSDef, translateFlowToTSDef} from '../../src';

type TestCase = {
  flowDef: string,
  name: string,
};

type TestOutput = {
  legacy: string,
  name: string,
  semantic: string,
};

const TEST_CASES: ReadonlyArray<TestCase> = [
  {
    name: 'named type alias',
    flowDef: 'declare type Foo = {}; declare export default Foo;',
  },
  {
    name: 'imported type',
    flowDef: "import type Foo from './Foo'; declare export default Foo;",
  },
  {
    name: 'typeof value',
    flowDef: 'declare const Foo: number; declare export default typeof Foo;',
  },
  {
    name: 'nullable named type',
    flowDef: 'declare type Foo = {}; declare export default ?Foo;',
  },
  {
    name: 'anonymous object',
    flowDef: 'declare export default {foo: string};',
  },
  {
    name: 'intersection',
    flowDef:
      'declare type A = {}; declare type B = {}; declare export default A & B;',
  },
  {
    name: 'qualified type',
    flowDef: 'declare export default React.ComponentType<{}>;',
  },
  {
    name: 'primitive type',
    flowDef: 'declare export default string;',
  },
  {
    name: 'recovery type',
    flowDef: 'declare export default $FlowFixMe;',
  },
];

test('default export naming strategies produce complete expected output', async () => {
  const outputs: Array<TestOutput> = [];

  for (const testCase of TEST_CASES) {
    const legacy = await translateFlowDefToTSDef(
      testCase.flowDef,
      prettierConfig,
    );
    const explicitLegacy = await translateFlowDefToTSDef(
      testCase.flowDef,
      prettierConfig,
      {useSemanticDefaultExportNames: false},
    );
    const semantic = await translateFlowDefToTSDef(
      testCase.flowDef,
      prettierConfig,
      {useSemanticDefaultExportNames: true},
    );

    expect(explicitLegacy).toBe(legacy);
    outputs.push({name: testCase.name, legacy, semantic});
  }

  expect(outputs).toMatchInlineSnapshot(`
   [
     {
       "legacy": "declare type Foo = {};
   declare const $$EXPORT_DEFAULT_DECLARATION$$: Foo;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "named type alias",
       "semantic": "declare type Foo = {};
   declare const $$Foo: Foo;
   declare type $$Foo = typeof $$Foo;
   export default $$Foo;
   ",
     },
     {
       "legacy": "import type Foo from './Foo';
   declare const $$EXPORT_DEFAULT_DECLARATION$$: Foo;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "imported type",
       "semantic": "import type Foo from './Foo';
   declare const $$Foo: Foo;
   declare type $$Foo = typeof $$Foo;
   export default $$Foo;
   ",
     },
     {
       "legacy": "declare const Foo: number;
   declare const $$EXPORT_DEFAULT_DECLARATION$$: typeof Foo;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "typeof value",
       "semantic": "declare const Foo: number;
   declare const $$Foo: typeof Foo;
   declare type $$Foo = typeof $$Foo;
   export default $$Foo;
   ",
     },
     {
       "legacy": "declare type Foo = {};
   declare const $$EXPORT_DEFAULT_DECLARATION$$: null | undefined | Foo;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "nullable named type",
       "semantic": "declare type Foo = {};
   declare const $$Foo: null | undefined | Foo;
   declare type $$Foo = typeof $$Foo;
   export default $$Foo;
   ",
     },
     {
       "legacy": "declare const $$EXPORT_DEFAULT_DECLARATION$$: {foo: string};
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "anonymous object",
       "semantic": "declare const $$default: {foo: string};
   declare type $$default = typeof $$default;
   export default $$default;
   ",
     },
     {
       "legacy": "declare type A = {};
   declare type B = {};
   declare const $$EXPORT_DEFAULT_DECLARATION$$: A & B;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "intersection",
       "semantic": "declare type A = {};
   declare type B = {};
   declare const $$default: A & B;
   declare type $$default = typeof $$default;
   export default $$default;
   ",
     },
     {
       "legacy": "import * as React from 'react';
   declare const $$EXPORT_DEFAULT_DECLARATION$$: React.ComponentType<{}>;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "qualified type",
       "semantic": "import * as React from 'react';
   declare const $$default: React.ComponentType<{}>;
   declare type $$default = typeof $$default;
   export default $$default;
   ",
     },
     {
       "legacy": "declare const $$EXPORT_DEFAULT_DECLARATION$$: string;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "primitive type",
       "semantic": "declare const $$default: string;
   declare type $$default = typeof $$default;
   export default $$default;
   ",
     },
     {
       "legacy": "declare const $$EXPORT_DEFAULT_DECLARATION$$: any;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
       "name": "recovery type",
       "semantic": "declare const $$default: any;
   declare type $$default = typeof $$default;
   export default $$default;
   ",
     },
   ]
  `);
});

test('translateFlowToTSDef forwards the naming strategy', async () => {
  const source = 'const Foo: number = 1; export default Foo;';
  const legacy = await translateFlowToTSDef(source, prettierConfig);
  const semantic = await translateFlowToTSDef(source, prettierConfig, {
    useSemanticDefaultExportNames: true,
  });

  expect({legacy, semantic}).toMatchInlineSnapshot(`
   {
     "legacy": "declare const Foo: number;
   declare const $$EXPORT_DEFAULT_DECLARATION$$: typeof Foo;
   declare type $$EXPORT_DEFAULT_DECLARATION$$ =
     typeof $$EXPORT_DEFAULT_DECLARATION$$;
   export default $$EXPORT_DEFAULT_DECLARATION$$;
   ",
     "semantic": "declare const Foo: number;
   declare const $$Foo: typeof Foo;
   declare type $$Foo = typeof $$Foo;
   export default $$Foo;
   ",
   }
  `);
});
