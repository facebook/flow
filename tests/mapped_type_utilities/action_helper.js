export function action<
  TArgs extends Object,
  TDefaults extends Partial<TArgs>,
  TResult,
>(
  defaults: TDefaults,
  create: (args: TArgs) => TResult,
): (Omit<TArgs, keyof TDefaults>) => TResult {
  throw 'unimplemented';
}
