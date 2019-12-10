/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';

/**
 * This codemod depends on ReactComponentExplicitTypeArgs! It assumes that the
 * code was first transformed using that.
 *
 * ```
 * class MyComponent extends React.Component<void, Props> {
 *   ...
 * }
 * ```
 *
 * ==>
 *
 * ```
 * class MyComponent extends React.Component<Props> {
 *   ...
 * }
 * ```
 *
 * See `./fixtures` for more examples.
 */
module.exports = (j, root) => {
  const ReactUtils = require('../../../codemods/ReactUtils')(j);
  const componentPattern = ReactUtils.getImportedComponentClassPattern(root);

  // Only proceed with the transform if we see that React is being used.
  if (!componentPattern) {
    return false;
  }

  root
    .find(j.ClassDeclaration, {
      superClass: componentPattern,
      superTypeParameters: {
        type: 'TypeParameterInstantiation',
      },
    })
    .forEach(handlePath);

  root
    .find(j.ClassExpression, {
      superClass: componentPattern,
      superTypeParameters: {
        type: 'TypeParameterInstantiation',
      },
    })
    .forEach(handlePath);

  function handlePath(path) {
    // Remove the first super type parameter.
    const defaultPropsType = path.node.superTypeParameters.params.shift();
    // If we have a default props type that is not `void` and a class body then
    // we might want to add the default props type to our class body.
    if (
      defaultPropsType &&
      defaultPropsType.type !== 'VoidTypeAnnotation' &&
      path.node.body
    ) {
      const body = path.node.body.body;
      // See if we already have a static default props class property.
      const hasDefaultProps = !!body.find(
        node =>
          node.type === 'ClassProperty' &&
          node.static === true &&
          node.key &&
          node.key.type === 'Identifier' &&
          node.key.name === 'defaultProps',
      );
      // If we do not have a static default props class property then we want to
      // add one to our class body using the default props type that we shifted.
      if (!hasDefaultProps) {
        body.unshift(
          j.classProperty(
            j.identifier('defaultProps'),
            null,
            j.typeAnnotation(defaultPropsType),
            true,
          ),
        );
      }
    }
  }

  return true;
};
