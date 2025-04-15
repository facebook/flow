// @flow

type T<X> = Partial<{ f: X }>;

type Props = { name: string, age: number };
declare var diff: Omit<Props, 'age'>;
diff.name; // this is required for the type to actually be evaluated
