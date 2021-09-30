/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('Prevent reposition loops', [
    addCode(`
      declare class Map<K,V> {
        get(k:K): ?V;
        set(k:K,v:V): void
      }
    `).noNewErrors(),

    addCode(`
      const map1 = new Map();
      map1.set('key', map1.get('key'));
    `).noNewErrors(),

    addCode(`
      const map2 = new Map();
      const val = map2.get('key');
      map2.set('key', val);
    `).noNewErrors(),
  ]),
]): Suite);
