/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Modifications Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the
 * License at http://www.apache.org/licenses/LICENSE-2.0
 * THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
 * WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
 * MERCHANTABILITY OR NON-INFRINGEMENT.
 * See the Apache Version 2.0 License for specific language governing permissions
 * and limitations under the License.
 *
 * @flow
 */
// @lint-ignore-every LICENSELINT
declare var undefined: void;

type $NotNullOrVoid =
| number
| string
| boolean
| interface {}
| ReadonlyArray<unknown>
| symbol
| bigint
| EnumValue<>
| Enum<>;

declare class CallSite {
    getThis(): any;
    getTypeName(): string;
    getFunction(): ?((...any) => any);
    getFunctionName(): string;
    getMethodName(): string;
    getFileName(): ?string;
    getLineNumber(): ?number;
    getColumnNumber(): ?number;
    getEvalOrigin(): ?CallSite;
    getScriptNameOrSourceURL(): ?string;
    isToplevel(): boolean;
    isEval(): boolean;
    isNative(): boolean;
    isConstructor(): boolean;
    toString(): string;
}

declare function $iterate<T>(p: Iterable<T>): T;

/* Type used internally for inferring the type of the yield delegate */
type $IterableOrAsyncIterableInternal<Input, out Yield, out Return, in Next> =
  Input extends AsyncIterable<any, any, any>
    ? AsyncIterable<Yield, Return, Next>
    : Iterable<Yield, Return, Next>;

// to be deprecated
declare class $ReadOnlyWeakMap<K extends WeaklyReferenceable, out V> {
    get(key: K): V | void;
    has(key: K): boolean;
}

type ReadonlyWeakMap<K extends WeaklyReferenceable, out V> = $ReadOnlyWeakMap<K, V>;


// to be deprecated
declare class $ReadOnlyWeakSet<T extends WeaklyReferenceable> {
    has(value: T): boolean;
}

type ReadonlyWeakSet<T extends WeaklyReferenceable> = $ReadOnlyWeakSet<T>;

/* CommonJS */

declare var global: any;

declare var module: {
    exports: any,
    require(id: string): any,
    id: string,
    filename: string,
    loaded: boolean,
    parent: any,
    children: Array<any>,
    path: string,
    paths: Array<string>,
    isPreloading: boolean,
    ...
};
declare var require: {
    (id: string): any,
    resolve: (id: string, options?: { paths?: Array<string>, ... }) => string,
    cache: any,
    main: typeof module,
    ...
};
declare var exports: {writeonly [key: string]: unknown};

/* Opaque type for module reference magic strings */
declare opaque type $Flow$ModuleRef<out T>;
declare opaque type $Flow$EsmModuleMarkerWrapperInModuleRef<out T>: T;
type $EnumProto<TEnum, TEnumValue, TRepresentationType> = {
  cast(this: TEnum, input: ?TRepresentationType): void | TEnumValue,
  getName(this: TEnum, input: TEnumValue): string,
  isValid(this: TEnum, input: ?TRepresentationType | TEnumValue): boolean,
  members(this: TEnum): IteratorObject<TEnumValue>,
  __proto__: null,
}

/**
 * String type whose values begin with the literal prefix `P`. The optional
 * second argument constrains what may follow the prefix.
 *
 * `StringPrefix<'data-'>` matches any string starting with `'data-'`.
 * `StringPrefix<'data-', 'foo' | 'bar'>` matches `'data-foo'` and `'data-bar'`.
 */
type StringPrefix<out P extends string, out R extends string = string> = `${P}${R}`;

/**
 * String type whose values end with the literal suffix `S`. The optional
 * second argument constrains what may precede the suffix.
 *
 * `StringSuffix<'-end'>` matches any string ending with `'-end'`.
 * `StringSuffix<'-end', 'foo' | 'bar'>` matches `'foo-end'` and `'bar-end'`.
 */
type StringSuffix<out S extends string, out R extends string = string> = `${R}${S}`;

/**
 * The possible underlying representation types of a Flow Enum.
 * For example, `enum E {A = 1}` has the representation type `number`.
 */
type EnumRepresentationTypes = string | number | symbol | boolean | bigint;

/**
 * A generic Flow Enum value. `EnumValue<>` represents any Flow Enum value.
 * You can supply a type argument to restrict to Flow Enums of a certain
 * representation type. For example:
 * ```
 * enum E {A = 1}
 * const b: EnumValue<number> = E.A; // Works
 * const a: EnumValue<> = E.A; // Works
 * ```
 */
type EnumValue<
  out TRepresentationType extends EnumRepresentationTypes = EnumRepresentationTypes
> = $EnumValue<TRepresentationType>;

type $EnumValueProto<TEnumObject, TRepresentationType> = {
  /**
   * Casts the Flow Enum value to its representation type.
   */
  valueOf(this: TEnumObject): TRepresentationType,
  __proto__: null,
};

/**
 * Represents a generic Flow Enum - the enum itself rather than its values.
 * You can supply a type argument to restrict to Flow Enums with a certain
 * enum value (take a look at the `EnumValue` type for more).
 * You can use the Flow Enum methods like `.cast` and `.members` on a value
 * of this type, but it does not have any specific members which you can access.
 */
type Enum<out TEnumValue extends EnumValue<> = EnumValue<>> = $Enum<TEnumValue>;

type Object$Assign = (target: any, ...source: ReadonlyArray<any>) => any;

/**
 * You can use this type instead of `any` to avoid triggering `unclear-type` error.
 * However, it's still a clear signal that you should use a better type.
 */
type $FlowFixMe = any;

type WithImplicitCoercion<T> = T | { valueOf(): T, ... };

type DistributiveOmit<T, K extends PropertyKey> = T extends any ? Omit<T, K> : empty;
type TupleIterator<out T> = IteratorObject<T>;
