export type Props<TValue> = Readonly<{
  onActivate?: (value: TValue) => unknown,
  value: TValue,
}>;

declare export default (component<TValue>(...props: Props<TValue>));
