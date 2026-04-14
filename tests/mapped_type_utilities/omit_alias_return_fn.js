function mergeWithDefaults<
  TArgs: interface {},
  TDefaults: Partial<TArgs>,
  TResult,
>(
  defaults: TDefaults,
  create: (args: TArgs) => TResult,
): (Omit<TArgs, keyof TDefaults>) => TResult {
  return args =>
    create(
      ({
        ...defaults,
        ...args,
      } as any), // ERROR
    );
}

const merged = mergeWithDefaults(
  {a: 1}, // ERROR
  ({a, b}: {a: number, b: string}) => b,
);

merged({b: 'ok'}) as string;
