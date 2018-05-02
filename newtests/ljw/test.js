/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({flowCmdOnly}) => [
  test('Standalone start', [flowCmdOnly(['check']).stdout('Found 0 errors')]),
]);
