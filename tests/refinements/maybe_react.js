const React = require('react');

type Node =
    null
  | boolean
  | number
  | string
  | React$Element<any>
  | Iterable<?React$Node>;

type Props = {|
  title?: ?number | Node,
|};

declare var x : Props;
const {title} = x;

((title != null ? title.toString() : '') : string)

type MaybeFun = ?(() => number);

declare const m: MaybeFun;
(m && m(): ?number); // OK!

type MaybeComponent = ?(React.AbstractComponent<{}>);

declare const C: MaybeComponent;
(C && <C />: ?React.Element<$NonMaybeType<MaybeComponent>>); // ok
