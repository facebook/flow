//@flow

type Props = {+foo: number};
function Component(x: Props): React$Node { return null; }

(Component: React$AbstractComponent<Props, void, React$Node>);
(Component: React$AbstractComponent<{}, void, React$Node>); // Error, missing foo
(Component: React$AbstractComponent<{+foo: number, +bar: number}, void, React$Node>); //Ok, extra prop bar
(Component: React$AbstractComponent<Props, number | void, React$Node>); // Ok
(Component: React$AbstractComponent<Props, number, React$Node>); // Error void ~> number

type Props2 = {foo: number, bar: number};
type Config2 = {+foo?: number, +bar: number};
function ComponentWithDefaultProps(x: Props2) { return null; }
ComponentWithDefaultProps.defaultProps = {foo: 3};

(ComponentWithDefaultProps: React$AbstractComponent<Config2, void, React$Node>);
(ComponentWithDefaultProps: React$AbstractComponent<{}, void, React$Node>); // Error, missing foo and bar
(ComponentWithDefaultProps: React$AbstractComponent<Config2, number | void, React$Node>); // Ok, instance is void
(ComponentWithDefaultProps: React$AbstractComponent<Config2, number, React$Node>); // Error, void ~> number

class NotAComponent {};

function NotAFunctionComponent(x: Props) {
  return NotAComponent; // Error, not a component
}

(NotAFunctionComponent: React$AbstractComponent<Props, void, React$Node>);
