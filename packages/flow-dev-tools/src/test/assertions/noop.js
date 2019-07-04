/**
 * @flow
 * @format
 */

import type {ErrorAssertionResult} from './assertionTypes';

export default function(): ErrorAssertionResult {
  return {type: 'pass'};
}
