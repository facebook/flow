/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

const NO_SERVER_RUNNING = 6;
const SERVER_RUNNING = 11;

export default suite(({addFile, removeFile, exitCode, flowCmd}) => [
  test('node - Adding a package.json should kill the server', [
    addFile('start.json', 'package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('node_flowconfig'),
  test('haste - Adding a package.json should kill the server', [
    addFile('start.json', 'package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('haste_flowconfig'),

  test('node - Removing a package.json should kill the server', [
    addFile('start.json', 'package.json'),
    removeFile('package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('node_flowconfig'),
  test('haste - Removing a package.json should kill the server', [
    addFile('start.json', 'package.json'),
    removeFile('package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing the name field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('nameChange.json', 'package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing the name field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('nameChange.json', 'package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing the main field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('mainChange.json', 'package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing the main field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('mainChange.json', 'package.json')
      .flowCmd(['status', '--no-auto-start'])
      .exitCodes([NO_SERVER_RUNNING])
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing an irrelevant field should NOT kill the server', [
    addFile('start.json', 'package.json'),
    addFile('irrelevantChange.json', 'package.json')
      .flowCmd(['start'])
      .exitCodes([SERVER_RUNNING])
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing an irrelevant field should NOT kill the server', [
    addFile('start.json', 'package.json'),
    addFile('irrelevantChange.json', 'package.json')
      .flowCmd(['start'])
      .exitCodes([SERVER_RUNNING])
  ]).flowConfig('haste_flowconfig'),
]);
