export type Props<TValue> = $ReadOnly<{
  onActivate?: (value: TValue) => unknown,
  value: TValue,
}>;

declare export default (component<TValue>(...props: Props<TValue>));
