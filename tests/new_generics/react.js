//@flow

const React = require('react');

type Props = $ReadOnly<{
  a: number,
  ...
}>;

function a<Props: Props>() {
  declare var a: React.ComponentType<Props>;
  declare var b: React.Component<{+a: number}>;
  (a: React.ComponentType<{a: number}>); // nope, contravariance
  (b: React.Component<Props>); // nope
}

type DefaultProps = {|
  b: string,
|};

function f<Config: Props>(
  InputComponent: React.AbstractComponent<Config, mixed>,
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

function HOC<Config, Instance>(
  x: React$AbstractComponent<Config, Instance>,
): React$AbstractComponent<Config, Instance> {
  return x;
}

function HOC2<Props: {}, DefaultProps: {}, Instance>(
  x: React$AbstractComponent<React$Config<Props, DefaultProps>, Instance>,
): React$AbstractComponent<React$Config<Props, DefaultProps>, Instance> {
  return x;
}

function WrapInDivWithExtraProp<Props, Instance>(
  X: React.AbstractComponent<{|...Props|}, Instance>,
): React.AbstractComponent<{|...Props, baz: number|}, void> {
  const C = ({baz, ...props}: {|...Props, baz: number|}) => (
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
  return function(props) {
    return <Component {...props} commentsConfig={42} />;
  };
}
