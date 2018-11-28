//@flow

type Props = {+foo: number};
function Component(x: Props): React$Node { return null; }

(Component: React$AbstractComponent<Props, void, void>);
(Component: React$AbstractComponent<{}, void, void>); // Error, missing foo
(Component: React$AbstractComponent<{+foo: number, +bar: number}, void, void>); //Ok, extra prop bar
(Component: React$AbstractComponent<Props, {foo: number}, void>); // Error, default props don't unify
(Component: React$AbstractComponent<Props, void, number | void>); // Ok
(Component: React$AbstractComponent<Props, void, number>); // Error void ~> number

type Props2 = {foo: number, bar: number};
type Config2 = {+foo?: number, +bar: number};
function ComponentWithDefaultProps(x: Props2) { return null; }
ComponentWithDefaultProps.defaultProps = {foo: 3};

(ComponentWithDefaultProps: React$AbstractComponent<Config2, {foo: number}, void>);
(ComponentWithDefaultProps: React$AbstractComponent<{}, {foo: number}, void>); // Error, missing foo and bar
(ComponentWithDefaultProps: React$AbstractComponent<Config2, {}, void>); // Error, default props don't unify
(ComponentWithDefaultProps: React$AbstractComponent<Config2, {foo: number, bar: number}, void>); // Error, default props don't unify
(ComponentWithDefaultProps: React$AbstractComponent<Config2, {foo: number}, number | void>); // Ok, instance is void
(ComponentWithDefaultProps: React$AbstractComponent<Config2, {foo: number}, number>); // Error, void ~> number

class NotAComponent {}; // Error, not a component

function NotAFunctionComponent(x: Props) {
  return NotAComponent;
}

(NotAFunctionComponent: React$AbstractComponent<Props, void, void>);
