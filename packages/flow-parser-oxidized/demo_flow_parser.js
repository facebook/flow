#!/usr/bin/env node
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';

// Point requires at the source files (same as Jest does)
const path = require('path');
const Module = require('module');

const forkSrc = path.resolve(__dirname, 'src');
const forkDist = path.resolve(__dirname, 'dist');

// Patch module resolution so the deserializer's `require('./FlowParserWASM')`
// finds the generated dist file instead of the .js.flow stub in src/.
const originalResolve = Module._resolveFilename;
Module._resolveFilename = function (request, parent, isMain, options) {
  if (request.endsWith('/FlowParserWASM') || request === './FlowParserWASM') {
    return path.join(forkDist, 'FlowParserWASM.js');
  }
  return originalResolve.call(this, request, parent, isMain, options);
};

// Need babel register for Flow syntax in the fork's src/
require('@babel/register')({
  presets: [['@babel/preset-env', {targets: {node: 'current'}}]],
  plugins: [
    '@babel/plugin-transform-flow-strip-types',
    'babel-plugin-transform-flow-enums',
  ],
  only: [forkSrc],
  ignore: [/node_modules/, /dist/],
});

const {parse} = require('./src/index');

// ============================================================
// Demo source files
// ============================================================

const demos = [
  {
    name: '1. Simple variable + function',
    source: `const greeting: string = "hello world";

function add(a: number, b: number): number {
  return a + b;
}`,
  },
  {
    name: '2. Type alias + generic',
    source: `type Result<T, E> = {
  ok: boolean,
  value?: T,
  error?: E,
};

type StringResult = Result<string, Error>;`,
  },
  {
    name: '3. Class with methods',
    source: `class Counter {
  count: number;

  constructor() {
    this.count = 0;
  }

  increment(): void {
    this.count++;
  }

  getCount(): number {
    return this.count;
  }
}`,
  },
  {
    name: '4. Import/export + arrow + JSX',
    source: `import type {Node} from 'ast';
import React from 'react';

export const Greeting = ({name}: {name: string}): React$Node => (
  <div className="greeting">
    <h1>Hello, {name}!</h1>
  </div>
);`,
  },
  {
    name: '5. Enum declaration',
    source: `enum Status {
  Active = 'active',
  Inactive = 'inactive',
  Pending = 'pending',
}`,
  },
  {
    name: '6. Interface + union + intersection',
    source: `interface Serializable {
  serialize(): string;
  deserialize(data: string): void;
}

type ID = string | number;
type Named = {name: string} & Serializable;`,
  },
  {
    name: '7. Async/await + try/catch + optional chaining',
    source: `async function fetchUser(id: number): Promise<?User> {
  try {
    const response = await fetch(\`/api/users/\${id}\`);
    const data = await response.json();
    return data?.user?.profile;
  } catch (e) {
    console.error(e);
    return null;
  }
}`,
  },
  {
    name: '8. Destructuring + spread + template literal',
    source: `function processConfig({host, port, ...rest}: Config): string {
  const merged = {...defaultConfig, ...rest, host, port};
  return \`\${host}:\${port}\`;
}`,
  },
  {
    name: '9. Declare class + typeof + tuple',
    source: `declare class EventEmitter {
  on(event: string, handler: Function): void;
  emit(event: string, ...args: Array<mixed>): void;
}

type Pair = [string, number];
type T = typeof EventEmitter;`,
  },
  {
    name: '10. Complex real-world module',
    source: `// @flow
import type {Config} from './config';
import {EventEmitter} from 'events';

type Listener<T> = (data: T) => void;

export class TypedEmitter<Events: {+[string]: mixed}> extends EventEmitter {
  on<K: $Keys<Events>>(event: K, listener: Listener<$ElementType<Events, K>>): this {
    return super.on(event, listener);
  }

  emit<K: $Keys<Events>>(event: K, data: $ElementType<Events, K>): boolean {
    return super.emit(event, data);
  }
}

export default function create<E: {+[string]: mixed}>(config?: Config): TypedEmitter<E> {
  const emitter = new TypedEmitter<E>();
  if (config?.debug === true) {
    emitter.on('error', (err) => console.error(err));
  }
  return emitter;
}`,
  },
];

// ============================================================
// Run the demos
// ============================================================

console.log('='.repeat(70));
console.log('  Flow Rust Parser - End-to-End Demo');
console.log('  Pure Rust -> WASM -> Binary Protocol -> JS Deserializer -> ESTree AST');
console.log('='.repeat(70));
console.log();

let passed = 0;
let failed = 0;

for (const demo of demos) {
  process.stdout.write(`${demo.name} ... `);
  try {
    const ast = parse(demo.source, {
      flow: 'all',
      tokens: false,
    });

    // Verify the result is a proper ESTree Program
    const checks = [];

    if (ast.type !== 'Program') {
      checks.push(`expected type=Program, got ${ast.type}`);
    }
    if (!Array.isArray(ast.body)) {
      checks.push('body is not an array');
    }
    if (ast.body.length === 0) {
      checks.push('body is empty');
    }
    // Check that every body node has a type
    for (let i = 0; i < ast.body.length; i++) {
      const node = ast.body[i];
      if (!node || !node.type) {
        checks.push(`body[${i}] has no type`);
      }
    }
    // Check locations are present
    if (!ast.loc || !ast.loc.start || !ast.loc.end) {
      checks.push('Program missing loc');
    }

    if (checks.length > 0) {
      console.log(`FAIL: ${checks.join('; ')}`);
      failed++;
    } else {
      const nodeTypes = ast.body.map(n => n.type).join(', ');
      console.log(`PASS (${ast.body.length} nodes: ${nodeTypes})`);
      passed++;
    }
  } catch (e) {
    console.log(`FAIL: ${e.message}`);
    failed++;
  }
}

console.log();
console.log('-'.repeat(70));
console.log();
console.log('Deep dive: sample FunctionDeclaration from the Flow Rust parser');
console.log();

const sampleSource = `
type Pair<A, B> = [A, B];

function swap<A, B>(pair: Pair<A, B>): Pair<B, A> {
  const [a, b] = pair;
  return [b, a];
}

export default swap;
`;

function collectNodeTypes(node, types = []) {
  if (!node || typeof node !== 'object') return types;
  if (node.type) types.push(node.type);
  for (const key of Object.keys(node)) {
    const val = node[key];
    if (Array.isArray(val)) {
      for (const child of val) {
        collectNodeTypes(child, types);
      }
    } else if (val && typeof val === 'object' && val.type) {
      collectNodeTypes(val, types);
    }
  }
  return types;
}

try {
  const flowAST = parse(sampleSource, {flow: 'all'});
  const flowTypes = collectNodeTypes(flowAST).sort();

  console.log(`  Flow Rust parser: ${flowAST.body.length} top-level nodes, ${flowTypes.length} total AST nodes`);
  console.log(`  Top-level: [${flowAST.body.map(n => n.type).join(', ')}]`);
  console.log();

  const flowFunc = flowAST.body.find(n => n.type === 'FunctionDeclaration');
  if (flowFunc) {
    console.log('  Sample FunctionDeclaration from Flow Rust parser:');
    console.log(`    id:         ${flowFunc.id?.name}`);
    console.log(`    async:      ${flowFunc.async}`);
    console.log(`    generator:  ${flowFunc.generator}`);
    console.log(`    params:     ${flowFunc.params?.length} params`);
    console.log(`    returnType: ${flowFunc.returnType?.typeAnnotation?.type}`);
    console.log(`    typeParams: ${flowFunc.typeParameters?.params?.length} type params`);
    console.log(`    body:       ${flowFunc.body?.body?.length} statements`);
    console.log(`    loc:        ${flowFunc.loc?.start?.line}:${flowFunc.loc?.start?.column} - ${flowFunc.loc?.end?.line}:${flowFunc.loc?.end?.column}`);
    passed++;
  }
} catch (e) {
  console.log(`  Deep dive FAILED: ${e.message}`);
  failed++;
}

console.log();
console.log('='.repeat(70));
console.log(`  Results: ${passed} passed, ${failed} failed`);
console.log('='.repeat(70));

process.exit(failed > 0 ? 1 : 0);
