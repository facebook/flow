// @flow

import * as React from 'react';

function ref_tests() {
  declare var Component1: React.AbstractComponent<{}, string>;
  <Component1 ref={(s) => (s: string | null)} />;
  declare class Component2 extends React.Component<{}> {}
  <Component2 ref={(s) => (s: Component2 | null)} />;
}

function cannot_resolve_name_regression_tests() {
  <div>{new Array<number>(1)}</div>; // ok
  <fbs>{new Array<number>(1)}</fbs>; // ok
  <fbt>{new Array<number>(1)}</fbt>; // ok
}

function react_abstract_component_subtyping() {
  const _: React.AbstractComponent<{foo: string}> = (props) => {
    (props.foo: string); // ok
    (props: empty); // error
  };
}

function jsx_function_children_ok() {
  declare function Child({foo: number}): React.Node;
  declare function Parent({children: (number) => React.Node}): React.Node;

  return <Parent>
    {n => <Child foo={(n: number)} /> /* OK */}
  </Parent>;
}

function jsx_function_children_error() {
  declare function Child({foo: number}): React.Node;
  declare function Parent({children: (number) => React.Node}): React.Node;

  return <Parent>
    {n => <Child foo={(n: empty)} /> /* ERROR */}
  </Parent>;
}
