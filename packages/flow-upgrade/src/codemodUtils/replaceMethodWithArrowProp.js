/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {MethodDefinition} from 'hermes-estree';
import type {TransformContext} from 'hermes-transform';

import {t} from 'hermes-transform';

/**
 * Converts method definition to an arrow function property
 * ```
 * method<T>(arg) { ... }
 * // to
 * +method = <T>(arg) => { ... };
 * ```
 */
export function replaceMethodWithArrowProp(
  context: TransformContext,
  method: MethodDefinition,
  name: string,
): void {
  context.replaceNode(
    method,
    t.PropertyDefinition({
      computed: false,
      declare: false,
      key: t.Identifier({
        name,
        // for types-first compliance, this should ideally be filled out for
        // non-private members. However it's *super* complicated to correctly
        // handle these cases statically (type parameters and default args make
        // it hard).
        // So instead we just ignore it and they can be filled later using a
        // type-aware codemod like
        // ```
        // flow codemod annotate-exports --write --munge-underscore-members --max-type-size 25
        // ```
        typeAnnotation: null,
      }),
      optional: false,
      static: false,
      value: t.ArrowFunctionExpression({
        async: method.value.async,
        body: method.value.body,
        params: method.value.params,
        predicate: method.value.predicate,
        typeParameters: method.value.typeParameters,
        returnType: method.value.returnType,
      }),
      // methods are readonly, so we mark the property as readonly as well
      variance: t.Variance({
        kind: 'plus',
      }),
    }),
  );
}
