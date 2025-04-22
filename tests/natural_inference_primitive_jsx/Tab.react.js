export type Props<TValue> = $ReadOnly<{
  onActivate?: (value: TValue) => mixed,
  value: TValue,
}>;

declare export default (component<TValue>(...Props<TValue>));
