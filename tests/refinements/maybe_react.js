const React = require('react');

type Node =
    null
  | boolean
  | number
  | string
  | React.JSX.Element
  | Iterable<?React.Node>;

type Props = {
  title?: ?number | Node,
};

declare const x : Props;
const {title} = x;

(title != null ? title.toString() : '') as string

type MaybeFun = ?(() => number);

declare const m: MaybeFun;
(m && m()) as ?number; // OK!

type MaybeComponent = ?(React.ComponentType<{...}>);

declare const C: MaybeComponent;
(C && <C />) as ?React.MixedElement; // ok
