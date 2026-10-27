/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

const assert = require('node:assert/strict');
const {execFileSync} = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {test} = require('node:test');
const vm = require('node:vm');
const {transformFileSync} = require('@babel/core');

const flowDir = path.resolve(__dirname, '../..');
const tslibPatchesDir = fs.mkdtempSync(
  path.join(os.tmpdir(), 'tslib_patches-'),
);
process.on('exit', () => fs.rmSync(tslibPatchesDir, {recursive: true}));
execFileSync(path.join(flowDir, 'scripts/apply-tslib-patches.sh'), [
  tslibPatchesDir,
]);
const loader = transformFileSync(
  path.resolve(__dirname, '../src/try-flow/flow-loader.js'),
  {
    configFile: false,
    babelrc: false,
    parserOpts: {flow: 'all'},
    presets: ['@babel/preset-flow'],
    plugins: [
      'babel-plugin-syntax-hermes-parser',
      '@babel/plugin-transform-modules-commonjs',
    ],
  },
).code;

function createLoader(missingUrl) {
  const registered = new Map();
  const requested = [];
  let builtins;
  const flow = {
    ready: Promise.resolve(),
    checkContent() {},
    registerFile(name, content) {
      registered.set(name, content);
    },
    initBuiltins(names) {
      builtins = Array.from(names);
    },
  };
  class XMLHttpRequest {
    open(method, url) {
      this.url = url;
      requested.push(url);
    }
    send() {
      if (this.url.startsWith('https://unpkg.com/')) {
        this.response = this.url;
        this.status = 200;
        queueMicrotask(() => this.onload());
        return;
      }
      const relative = this.url
        .replace('/flow/master/flowlib/', 'lib/')
        .replace('/flow/master/tslib/', 'tslib/')
        .replace('/flow/master/tslib_patches/', `${tslibPatchesDir}/`);
      try {
        this.response = fs.readFileSync(
          path.resolve(flowDir, relative),
          'utf8',
        );
        this.status = this.url === missingUrl ? 404 : 200;
      } catch (error) {
        if (error.code !== 'ENOENT') throw error;
        this.status = 404;
      }
      this.statusText = this.status === 200 ? 'OK' : 'Not Found';
      queueMicrotask(() => this.onload());
    }
  }
  const context = {
    exports: {},
    require() {},
    requirejs(paths, resolve) {
      resolve(flow);
    },
    XMLHttpRequest,
  };
  vm.runInNewContext(loader, context);
  return {
    load: (version = 'master') => context.exports.load(url => url, version),
    registered,
    requested,
    getBuiltins: () => builtins,
  };
}

// Every master builtin must resolve to a definition present in the checkout.
test('loads master builtins from the current Flow and TypeScript libraries', async () => {
  const {load, registered, getBuiltins} = createLoader();
  await load();
  for (const name of [
    'lib.es2018.asyncgenerator',
    'lib.es2018.asynciterable',
  ]) {
    const filename = `/flow/master/flowlib/${name}.js`;
    assert.equal(
      registered.get(filename),
      fs.readFileSync(path.join(flowDir, `tslib/${name}.d.ts`), 'utf8'),
    ); // OK
    assert.equal(getBuiltins().includes(filename), true); // OK
  }
  for (const name of ['lib.es2016.array.include', 'lib.es2019.array']) {
    const filename = `/flow/master/flowlib/${name}.js`;
    assert.equal(
      registered.get(filename),
      fs.readFileSync(path.join(tslibPatchesDir, `${name}.d.ts`), 'utf8'),
    ); // OK
    assert.equal(getBuiltins().includes(filename), true); // OK
  }
  assert.equal(
    getBuiltins().every(name => registered.has(name)),
    true,
  ); // OK
});

// Released async definitions stay in flowlib; their TypeScript migration starts in 0.338.
test('uses version-appropriate async library definitions', async () => {
  for (const [version, releaseVersion, directory, extension] of [
    ['0.333.0', '0.333.0', 'flowlib', 'js'],
    ['0.336.0', '0.336.0', 'flowlib', 'js'],
    ['v0.337.0', '0.337.0', 'flowlib', 'js'],
    ['0.338.0', '0.338.0', 'tslib', 'd.ts'],
    ['v0.338.0-rc.1', '0.338.0-rc.1', 'tslib', 'd.ts'],
  ]) {
    const {load, requested, registered} = createLoader();
    await load(version);
    const baseUrl = `https://unpkg.com/try-flow-website-js@${releaseVersion}`;
    for (const name of [
      'lib.es2018.asyncgenerator',
      'lib.es2018.asynciterable',
    ]) {
      const sourceUrl = `${baseUrl}/${directory}/${name}.${extension}`;
      assert.equal(requested.includes(sourceUrl), true); // OK
      assert.equal(
        registered.get(
          `/try-flow-website-js@${releaseVersion}/flowlib/${name}.js`,
        ),
        sourceUrl,
      ); // OK
    }
  }
});

// A failed builtin request reports the actual requested URL and HTTP status.
test('includes the failed builtin URL and status in load errors', async () => {
  const url = '/flow/master/tslib/lib.es2018.asyncgenerator.d.ts';
  const {load} = createLoader(url);
  await assert.rejects(load(), {
    message: `Failed to load ${url}: 404 Not Found`,
  }); // OK
});
