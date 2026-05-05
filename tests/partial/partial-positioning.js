export type BaseType = boolean | number; 

type RecursiveProp =
  | BaseType
  | Partial<{[string]: RecursiveProp}>
  | ReadonlyArray<RecursiveProp>;

type Props = Partial<{[string]: RecursiveProp}>;

type Provider<T> = () => T => null;

declare export function register<T extends Props>(
 x: Provider<T>,
): void;


// All errors should be positioned here, not at any definitions
register(() => (x: {foo: number}) => null); // ERROR!
