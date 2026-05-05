function projectWithoutA<T extends {a: number, b: string}>(): Omit<T, 'a'> {
  throw 'unimplemented';
}

projectWithoutA() as {b: string}; // ERROR

function actionWithDefaults<
  TArgs extends Object,
  TDefaults extends Partial<TArgs>,
  TResult,
>(
  defaults: TDefaults,
  create: (args: TArgs) => TResult,
): (Omit<TArgs, keyof TDefaults>) => TResult {
  throw 'unimplemented';
}

actionWithDefaults(
  {a: 1},
  ({a, b}) => b,
) as ({b: string}) => string; // ERROR

function selectProvidedKey<
  TArgs extends Object,
  TDefaults extends Partial<TArgs>,
>(
  defaults: TDefaults,
  key: keyof TDefaults,
): keyof TDefaults {
  return key;
}

selectProvidedKey({a: 1}, 'a') as 'a';

function actionWithoutLiteralKey<
  TArgs extends Object,
  TResult,
>(
  create: (args: TArgs) => TResult,
): (Omit<TArgs, 'a'>) => TResult {
  throw 'unimplemented';
}

actionWithoutLiteralKey(
  ({a, b}) => b,
) as ({b: string}) => string;

type HasB = {
  a: string,
  b: number,
};

declare function acceptsOmit<T>(value: Omit<T, 'b'>): void;

acceptsOmit<HasB>({a: 'x'});
