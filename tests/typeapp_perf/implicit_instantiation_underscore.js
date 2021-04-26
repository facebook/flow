//@flow

// This test exercises the instantiation cache when explicit type arguments are provided.
// Prior to the introduction of `_`, there were no known ways to introduce non-termination
// when explicit type arguments are supplied. Since `_` is equivalent to omitting the type
// arguments entirely, this introduces the full space of non-termination issues to cals
// with explicit type arguments.

export type Obj = $ReadOnly<{||}>;

declare class ImmutableMap<K, +V> {
  static <K, V>(): ImmutableMap<K, V>;
  update<V_>(key: K, updater: (value: V) => V_): ImmutableMap<K, V | V_>;
};

type Props = {
  items: $ReadOnlyArray<Obj>,
  stringOfObj: Obj => ?string,
};

declare var props: Props;

const groups = props.items.reduce((map, item) => {
  const group = props.stringOfObj(item);
  return map.update<_>(group, items => items.push(item));
}, ImmutableMap());
