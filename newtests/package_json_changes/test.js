/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, removeFile, exitCode, flowCmd}) => [
  test('node - Adding a package.json should kill the server', [
    addFile('start.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('node_flowconfig'),
  test('haste - Adding a package.json should kill the server', [
    addFile('start.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('haste_flowconfig'),

  test('node - Removing a package.json should kill the server', [
    addFile('start.json', 'package.json'),
    removeFile('package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('node_flowconfig'),
  test('haste - Removing a package.json should kill the server', [
    addFile('start.json', 'package.json'),
    removeFile('package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing the name field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('nameChange.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing the name field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('nameChange.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing the main field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('mainChange.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing the main field should kill the server', [
    addFile('start.json', 'package.json'),
    addFile('mainChange.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(false)
  ]).flowConfig('haste_flowconfig'),

  test('node - Changing an irrelevant field should NOT kill the server', [
    addFile('start.json', 'package.json'),
    addFile('irrelevantChange.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(true)
  ]).flowConfig('node_flowconfig'),
  test('haste - Changing an irrelevant field should NOT kill the server', [
    addFile('start.json', 'package.json'),
    addFile('irrelevantChange.json', 'package.json')
      .waitForServerToDie(2000)
      .serverRunning(true)
  ]).flowConfig('haste_flowconfig'),
]);
