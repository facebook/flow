//@flow

const React = require('react');

type Props = $ReadOnly<{
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

function f<Config: Props>(
  InputComponent: React.ComponentType<Config>,
): React.ComponentType<React.Config<Config, DefaultProps>> {
  return class extends React.Component<React.Config<Config, DefaultProps>> {
    f() {
      const inputComponent = <InputComponent {...this.props} />;
    }
  };
}

function connect<TProps: {...}, SProps: TProps>(): React.ComponentType<SProps> {
  return class extends React.Component<TProps> {};
}

function hoc<Props, Component: React.ComponentType<Props>>(
  WrappedComponent: Component,
): React.ComponentType<React.ElementConfig<Component>> {
  return (props: Props) => <WrappedComponent {...props} />;
}

function HOC<Config: {...}, Instance>(
  x: component(ref: React.RefSetter<Instance>, ...Config),
): component(ref: React.RefSetter<Instance>, ...Config) {
  return x;
}

function HOC2<Props: {...}, DefaultProps: {}, Instance>(
  x: component(ref: React.RefSetter<Instance>, ...React.Config<Props, DefaultProps>),
): component(ref: React.RefSetter<Instance>, ...React.Config<Props, DefaultProps>) {
  return x;
}

function WrapInDivWithExtraProp<Props: {...}, Instance>(
  X: component(ref: React.RefSetter<Instance>, ...Props),
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

function mapProps<InputProps, SubInputProps: InputProps, OutputProps>(
  mapperFn: InputProps => OutputProps,
): (React.ComponentType<OutputProps>) => React.ComponentType<SubInputProps> {
  return Component => props => <Component {...mapperFn(props)} />;
}

export type PCCP = {
  commentsConfig: number,
  ...
};

export function withCommentsConfig<TProps: PCCP>(
  Component: React.ComponentType<TProps>,
): React.ComponentType<$Diff<TProps, PCCP>> {
  return function (props) {
    return <Component {...props} commentsConfig={42} />;
  };
}
