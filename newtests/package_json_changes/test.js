/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, removeFile, exitCode, flowCmd}) => [
  test('node - Adding a package.json should kill the server', [
    addFile('start.json', 'package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('node_flowconfig'),
  test('haste - Adding a package.json should kill the server', [
    addFile('start.json', 'package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('haste_flowconfig'),

  test('node - Removing a package.json should kill the server', [
    addFile('start.json', 'package.json'),
    removeFile('package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('node_flowconfig'),
  test('haste - Removing a package.json should kill the server', [
    addFile('start.json', 'package.json'),
    removeFile('package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing the name field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('nameChange.json', 'package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing the name field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('nameChange.json', 'package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing the main field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('mainChange.json', 'package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing the main field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('mainChange.json', 'package.json')
      .startFlowServer()
      .waitUntilServerStatus(10000, 'stopped')
      .verifyServerStatus('stopped'),
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing an irrelevant field should NOT kill the server', [
    addFile('start.json', 'package.json'),
    addFile('irrelevantChange.json', 'package.json')
      .startFlowServer() // makes this step start Flow before irrelevantChange is added
      .waitUntilServerStatus(2000, 'stopped') // only 2s not 10s so as not to waste time
      .verifyServerStatus('running'),
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing an irrelevant field should NOT kill the server', [
    addFile('start.json', 'package.json'),
    addFile('irrelevantChange.json', 'package.json')
      .startFlowServer() // makes this step start Flow before irrelevantChange is added
      .waitUntilServerStatus(2000, 'stopped') // only 2s not 10s so as not to waste time
      .verifyServerStatus('running'),
  ]).flowConfig('haste_flowconfig'),
]);
