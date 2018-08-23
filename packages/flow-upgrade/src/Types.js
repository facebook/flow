/**
 * @format
 * @flow
 */

export type Upgrade = CodemodUpgrade;

export type CodemodUpgrade = {|
  +kind: 'codemod',
  +title: string,
  +description: string,
  +transformPath: string,
|};
