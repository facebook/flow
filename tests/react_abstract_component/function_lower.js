//@flow

type Props = {+foo: number};
function Component(x: Props): React$Node {
  return null;
}

Component as React$AbstractComponent<Props, void, React$Node>;
Component as React$AbstractComponent<{}, void, React$Node>; // Error, missing foo
Component as React$AbstractComponent<
  {+foo: number, +bar: number},
  void,
  React$Node,
>; //Ok, extra prop bar
Component as React$AbstractComponent<Props, number | void, React$Node>; // Ok
Component as React$AbstractComponent<Props, number, React$Node>; // Error void ~> number

type Props2 = {foo: number, bar: number};
type Config2 = {+foo?: number, +bar: number};
function ComponentWithDefaultProps(x: Props2) {
  return null;
}
ComponentWithDefaultProps.defaultProps = {foo: 3};

ComponentWithDefaultProps as React$AbstractComponent<Config2, void, React$Node>;
ComponentWithDefaultProps as React$AbstractComponent<{}, void, React$Node>; // Error, missing foo and bar
ComponentWithDefaultProps as React$AbstractComponent<
  Config2,
  number | void,
  React$Node,
>; // Ok, instance is void
ComponentWithDefaultProps as React$AbstractComponent<
  Config2,
  number,
  React$Node,
>; // Error, void ~> number

class NotAComponent {}

function NotAFunctionComponent(x: Props) {
  return NotAComponent; // Error, not a component
}

NotAFunctionComponent as React$AbstractComponent<Props, void, React$Node>;

function SpecificRender(): number {
  return 3;
}
SpecificRender as React$AbstractComponent<{}, void, number>;
SpecificRender as React$AbstractComponent<{}, void, React$Node>; // OK, covariant type argument
SpecificRender as React$AbstractComponent<{}, void, string>; // ERROR, number ~> string
