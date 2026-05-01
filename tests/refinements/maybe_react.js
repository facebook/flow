const React = require('react');

type Node =
    null
  | boolean
  | number
  | string
  | ExactReactElement_DEPRECATED<any>
  | Iterable<?React.Node>;

type Props = {|
  title?: ?number | Node,
|};

declare var x : Props;
const {title} = x;

(title != null ? title.toString() : '') as string

type MaybeFun = ?(() => number);

declare const m: MaybeFun;
(m && m()) as ?number; // OK!

type MaybeComponent = ?(React.ComponentType<{}>);

declare const C: MaybeComponent;
(C && <C />) as ?ExactReactElement_DEPRECATED<$NonMaybeType<MaybeComponent>>; // ok
