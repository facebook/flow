//@flow

import * as React from 'react';

class Component extends React.Component<{|foo: number, bar: number|}> {
  static defaultProps: {| foo: number |} = {foo: 3};
}

function TrivialHOC<Props, DefaultProps: ?{}, Instance>(
  x: React.AbstractComponent<Props, DefaultProps, Instance>,
): React.AbstractComponent<Props, DefaultProps, Instance> {
  return x;
}

const TrivialWrap = TrivialHOC(Component);
// Note: Default props still intact
(TrivialWrap: React.AbstractComponent<{|foo: number, bar: number|}, {| foo: number |}, Component>);

function WrapInDivWithExtraProp<Props, DefaultProps, Instance>(
  x: React.AbstractComponent<Props, DefaultProps, Instance>,
): React.AbstractComponent<{| ...Props, baz: number |}, DefaultProps, void> {
  const C = (props: {|...Props, baz: number|}) =>
    <div>
      {props.baz}
      <x {...props} />
    </div>;
  C.defaultProps = {...x.defaultProps};
  return C;
}

const WrappedInDivWithExtraProp = WrapInDivWithExtraProp(Component); // Note, we lose instance type here
(WrappedInDivWithExtraProp: React.AbstractComponent<{| foo: number, bar: number, baz: number |}, {| foo: number |}, void>);

function AddPropWithDefault<Props, DefaultProps, Instance>(
  x: React.AbstractComponent<Props, DefaultProps, Instance>
): React.AbstractComponent<{| ...Props, baz:number |}, {| ...DefaultProps, baz: number |}, void> {
  const C = (props: {| ...Props, baz: number |}) =>
    <div>
      {props.baz}
      <x {...props} />
    </div>;
  C.defaultProps = {...x.defaultProps, baz: 3};
  return C;
}

const WrappedAddPropWithDefault = AddPropWithDefault(Component);
(WrappedAddPropWithDefault: React.AbstractComponent<
  {| foo: number, bar: number, baz: number |},
  {| foo: number, baz: number |},
  void,
 >);
