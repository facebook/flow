// @flow

// --- Utility types ---

type User = {
  name: string,
  age: number,
  email: string,
  isAdmin: boolean,
};

// Readonly utility
type ReadOnlyUser = Readonly<User>;

// Partial utility
type PartialUser = Partial<User>;

// Required utility
type RequiredUser = Required<{name?: string, age?: number}>;

// Pick utility
type UserSummary = Pick<User, 'name' | 'email'>;

// Omit utility
type UserWithoutEmail = Omit<User, 'email'>;

// Keys and Values
type UserKeys = keyof User;
type UserValues = Values<User>;

// Record utility
type StatusMap = Record<'active' | 'inactive' | 'pending', number>;

// NonNullable
type MaybeString = ?string;
type DefinitelyString = NonNullable<MaybeString>;

// Exclude and Extract
type AllStatus = 'active' | 'inactive' | 'pending' | 'deleted';
type ActiveStatus = Exclude<AllStatus, 'deleted'>;
type DangerStatus = Extract<AllStatus, 'inactive' | 'deleted'>;

// ReturnType and Parameters
type SampleFn = (x: string, y: number) => boolean;
type SampleReturn = ReturnType<SampleFn>;
type SampleParams = Parameters<SampleFn>;

// Class utility
class Animal {
  name: string;
  constructor(name: string) {
    this.name = name;
  }
}
type AnimalClass = Class<Animal>;

// $Exact utility
type InexactBase = {name: string, age: number, ...};
type ExactFromInexact = $Exact<InexactBase>;

// $KeyMirror utility
type KeyMirrorExample = $KeyMirror<{foo: number, bar: string}>;

// --- Intersection types ---

type HasName = {name: string};
type HasAge = {age: number};
type Named = HasName & HasAge;

// --- Tuple types ---

type Pair = [string, number];
type LabeledTuple = [name: string, age: number, active: boolean];
type ReadOnlyTuple = Readonly<[string, number, boolean]>;
type OptionalTuple = [required: string, optional?: number];

// --- Promise and async ---

type StringPromise = Promise<string>;
type NestedPromise = Promise<Promise<number>>;

// --- Readonly collections ---

type ReadOnlyArray_t = ReadonlyArray<number>;
type ReadOnlyMap_t = ReadonlyMap<string, number>;
type ReadOnlySet_t = ReadonlySet<string>;

// --- Interfaces ---

interface Serializable {
  serialize(): string;
  deserialize(data: string): void;
}

interface Comparable<T> {
  compareTo(other: T): number;
}

interface Printable extends Serializable {
  prettyPrint(): string;
}

// --- Opaque types ---

opaque type UserID = string;
opaque type Timestamp: number = number;

// --- Complex generics ---

type Result<T, E> = {ok: true, value: T} | {ok: false, error: E};

type TreeNode<T> = {
  value: T,
  children: Array<TreeNode<T>>,
};

// --- Mapped types ---

type MakeReadonly<T: {...}> = {+[K in keyof T]: T[K]};
type MakeOptional<T: {...}> = {[K in keyof T]+?: T[K]};
type Methodify<T: {...}> = {[K in keyof T]: () => T[K]};

// --- Conditional types ---

type UnwrapPromise<T> = T extends Promise<infer V> ? V : T;
type UnwrapArray<T> = T extends ReadonlyArray<infer V> ? V : T;
type IsString<T> = T extends string ? true : false;

// --- Class with generics ---

class Container<T> {
  value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<U>(fn: (T) => U): Container<U> {
    return new Container(fn(this.value));
  }
}

// --- Complex function types ---

type Middleware<S, A> = (state: S, action: A, next: (A) => S) => S;
type Callback<T> = (error: Error | null, result: T) => void;
type AsyncFn<T> = () => Promise<T>;

// --- Nested object types ---

type DeepConfig = {
  database: {
    host: string,
    port: number,
    credentials: {
      username: string,
      password: string,
    },
  },
  cache: {
    ttl: number,
    maxSize: number,
  },
  features: Readonly<{
    enableLogging: boolean,
    enableMetrics: boolean,
  }>,
};

// --- Exact vs inexact object ---

type ExactObj = {|name: string, age: number|};
type InexactObj = {name: string, age: number, ...};

// --- Component using complex types ---

component ComplexComponent(
  user: ReadOnlyUser,
  config: DeepConfig,
  result: Result<string, Error>,
) {
  return null;
}

// --- Union of literals with many members (short names -> fits on line) ---

type Direction = 'n' | 's' | 'e' | 'w' | 'ne' | 'nw' | 'se' | 'sw';

// --- Indexed access types ---

type DatabaseHost = DeepConfig['database']['host'];
type CacheTTL = DeepConfig['cache']['ttl'];

// --- Typeof ---

declare var sampleConfig: DeepConfig;
type ConfigType = typeof sampleConfig;

// --- StringPrefix and StringSuffix ---

type DataAttr = StringPrefix<'data-'>;
type ExclaimStr = StringSuffix<'!'>;
type CSSVar = StringPrefix<'var(--'> & StringSuffix<')'>;

// --- Mapped type over arrays ---

type NumberTuple = [number, number, number];
type StringifiedTuple = {[K in keyof NumberTuple]: string};

// --- Validators using mapped types ---

type Validators<T: {...}> = {[K in keyof T]: (value: T[K]) => boolean};

// Exports
export type {
  User, ReadOnlyUser, PartialUser, RequiredUser, UserSummary, UserWithoutEmail,
  UserKeys, UserValues, StatusMap,
  MaybeString, DefinitelyString, ActiveStatus, DangerStatus,
  SampleReturn, SampleParams, AnimalClass,
  ExactFromInexact, KeyMirrorExample,
  HasName, HasAge, Named,
  Pair, LabeledTuple, ReadOnlyTuple, OptionalTuple,
  StringPromise, NestedPromise,
  ReadOnlyArray_t, ReadOnlyMap_t, ReadOnlySet_t,
  Serializable, Comparable, Printable,
  UserID, Timestamp,
  Result, TreeNode, MakeReadonly, MakeOptional, Methodify,
  UnwrapPromise, UnwrapArray, IsString,
  Middleware, Callback, AsyncFn, DeepConfig,
  ExactObj, InexactObj, Direction,
  DatabaseHost, CacheTTL, ConfigType,
  DataAttr, ExclaimStr, CSSVar,
  NumberTuple, StringifiedTuple, Validators,
};
export {Container, ComplexComponent, Animal};
