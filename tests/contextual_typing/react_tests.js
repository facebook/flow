// @flow

import * as React from 'react';

function ref_tests() {
  declare var Component1: React.AbstractComponent<{}, string>;
  <Component1 ref={(s) => (s: string | null)} />;
  declare class Component2 extends React.Component<{}> {}
  <Component2 ref={(s) => (s: Component2 | null)} />;
}
