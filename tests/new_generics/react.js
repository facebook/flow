//@flow

const React = require('react');

type Props = Readonly<{
  a: number,
  ...
}>;

function a<Props: Props>() {
  declare var a: React.ComponentType<Props>;
  declare var b: React.Component<{+a: number}>;
  a as React.ComponentType<{a: number}>; // nope, contravariance
  b as React.Component<Props>; // nope
}

type DefaultProps = {|
  b: string,
|};

function connect<TProps: {...}, SProps: TProps>(): component(ref?: React.RefSetter<mixed>, ...SProps) {
  return class extends React.Component<TProps> {};
}

function hoc<Props: {...}, Component: component(...Props)>(
  WrappedComponent: Component,
): React.ComponentType<React.ElementConfig<Component>> {
  return (props: Props) => <WrappedComponent {...props} />;
}

function HOC<Config: {...}, Instance>(
  x: component(ref: React.RefSetter<Instance>, ...Config),
): component(ref: React.RefSetter<Instance>, ...Config) {
  return x;
}

function WrapInDivWithExtraProp<Props: {...}>(
  X: component(...Props),
): component(...{|...$Exact<Props>, baz: number|}) {
  const C = ({baz, ...props}: {|...$Exact<Props>, baz: number|}) => (
    <div>
      {baz}
      <X {...props} />
    </div>
  );
  C.defaultProps = {...X.defaultProps}; // nope
  return C;
}

function mapProps<InputProps: {...}, SubInputProps: InputProps, OutputProps: {...}>(
  mapperFn: InputProps => OutputProps,
): (component(...OutputProps)) => component(...SubInputProps) {
  return Component => props => <Component {...mapperFn(props)} />;
}

export type PCCP = {
  commentsConfig: number,
  ...
};

export function withCommentsConfig<TProps: PCCP>(
  Component: component(...TProps),
): component(...Omit<TProps, keyof PCCP>) {
  return function (props) {
    return <Component {...props} commentsConfig={42} />;
  };
}
