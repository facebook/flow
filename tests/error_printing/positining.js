//@flow

'use strict';

const React = require('React');

type $FragmentRef<T> = {
  $fragmentRefs: $PropertyType<T, '$refType'>,
};

type X = $NonMaybeType<{|
  +$refType: 3,
|}>;

type $RelayProps<Props, RelayPropT = {}> = $ObjMap<
  $Diff<Props, {relay: {} | void, ...}>,
  (<T: {+$refType: empty, ...}>(T) => T) &
    (<T: {+$refType: empty, ...}>(?T) => ?T) &
    (<TRef: 3, T: {+$refType: TRef, ...}>(T) => $FragmentRef<T>),
>;

type Props = {
  selectedValue: X,
};

declare var x: $RelayProps<React$ElementConfig<(Props) => React.Node>, {}>;

(x: $RelayProps<Props>);
