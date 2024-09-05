//@flow

'use strict';

const React = require('react');

type $FragmentRef<T> = {
  $fragmentRefs: $PropertyType<T, '$refType'>,
};

type X = $NonMaybeType<{|
  +$refType: 3,
|}>;

type $RelayProps<Props, RelayPropT = {}> = MapProps<
  $Diff<Props, {relay: {} | void, ...}>,
>;
type MapProps<O> = {
  [K in keyof O]: O[K] extends {+$refType: empty, ...} ? O[K] : O[K] extends ?{+$refType: 3, ...} ? $FragmentRef<O[K]> : empty
};

type Props = {
  selectedValue: X,
};

declare var x: $RelayProps<React.ElementConfig<(Props) => React.Node>, {}>;

x as $RelayProps<Props>;

declare class Map<V> {
  forEach<X>(callbackfn: (map: Map<V>) => mixed): void;
  get(): V | void;
  set(value: V): Map<V>;
}

class A {}

var m = new Map<Array<string>>();
var a = m.get();
a.push('');

m.forEach(map => {
  map.set(new A());
});
