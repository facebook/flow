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
interface ClassMethodDecoratorContext<
  This = any,
  Value extends (this: This, ...args: Array<any>) => any = (this: This, ...args: Array<any>) => any,
> {
  readonly kind: "method";
  readonly name: string | symbol;
  readonly static: boolean;
  readonly private: boolean;
  readonly access: { has(object: This): boolean, get(object: This): Value, ... };
  addInitializer(initializer: (this: This) => void): void;
}

interface ClassGetterDecoratorContext<This = any, Value = any> {
  readonly kind: "getter";
  readonly name: string | symbol;
  readonly static: boolean;
  readonly private: boolean;
  readonly access: { has(object: This): boolean, get(object: This): Value, ... };
  addInitializer(initializer: (this: This) => void): void;
}

interface ClassSetterDecoratorContext<This = any, Value = any> {
  readonly kind: "setter";
  readonly name: string | symbol;
  readonly static: boolean;
  readonly private: boolean;
  readonly access: { has(object: This): boolean, set(object: This, value: Value): void, ... };
  addInitializer(initializer: (this: This) => void): void;
}

interface ClassFieldDecoratorContext<This = any, Value = any> {
  readonly kind: "field";
  readonly name: string | symbol;
  readonly static: boolean;
  readonly private: boolean;
  readonly access: { has(object: This): boolean, get(object: This): Value, set(object: This, value: Value): void, ... };
  addInitializer(initializer: (this: This) => void): void;
}

interface ClassAccessorDecoratorContext<This = any, Value = any> {
  readonly kind: "accessor";
  readonly name: string | symbol;
  readonly static: boolean;
  readonly private: boolean;
  readonly access: { has(object: This): boolean, get(object: This): Value, set(object: This, value: Value): void, ... };
  addInitializer(initializer: (this: This) => void): void;
}

interface ClassAccessorDecoratorTarget<This, Value> {
  get: (this: This) => Value;
  set: (this: This, value: Value) => void;
}

interface ClassAccessorDecoratorResult<This, Value> {
  get?: (this: This) => Value;
  set?: (this: This, value: Value) => void;
  init?: (this: This, value: Value) => Value;
}

type DecoratorContext =
  | ClassMethodDecoratorContext<any, any>
  | ClassGetterDecoratorContext<any, any>
  | ClassSetterDecoratorContext<any, any>
  | ClassFieldDecoratorContext<any, any>
  | ClassAccessorDecoratorContext<any, any>;
