/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';

const LIFECYCLE_METHODS = new Map([
  ['componentWillReceiveProps', ['props']],
  ['shouldComponentUpdate', ['props', 'state']],
  ['componentWillUpdate', ['props', 'state']],
  ['componentDidUpdate', ['props', 'state']],
]);

/**
 * ```
 * class MyComponent extends React.Component {
 *   props: Props;
 *   ...
 * }
 * ```
 *
 * ==>
 *
 * ```
 * class MyComponent extends React.Component<void, Props> {
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

  // The fallback type annotations we will use if we can't find anything.
  const fallbackDefaultPropsTypeAnnotation = j.genericTypeAnnotation(
    j.identifier('$FlowFixMeDefaultProps'),
    null,
  );
  const fallbackPropsTypeAnnotation = j.genericTypeAnnotation(
    j.identifier('$FlowFixMeProps'),
    null,
  );
  const fallbackStateTypeAnnotation = j.genericTypeAnnotation(
    j.identifier('$FlowFixMeState'),
    null,
  );

  root
    .find(j.ClassDeclaration, {superClass: componentPattern})
    .filter(path => !path.node.superTypeParameters)
    .forEach(handlePath);

  root
    .find(j.ClassExpression, {superClass: componentPattern})
    .filter(path => !path.node.superTypeParameters)
    .forEach(handlePath);

  return true;

  function handlePath(path) {
    // Initialize the type annotations for the three generic parameters we
    // will need to pass into `Component`. `defaultProps` and `state` default
    // to void. If we can't find a type we assume they do not exist. `props`,
    // however, we set to null initially as we have a few fallback mechanisms.
    let defaultPropsTypeAnnotation = j.voidTypeAnnotation();
    let propsTypeAnnotation = null;
    let stateTypeAnnotation = null;
    // Some components use the constructor to type props. If they do we want
    // to remember that, but we do not want to override any value in
    // `propsTypeAnnotation`.
    let constructorPropsTypeAnnotation = null;
    // We want to try to get the props and state type annotation from lifecycle
    // methods as well.
    let propsLifecycleTypeAnnotation = null;
    let stateLifecycleTypeAnnotation = null;
    // If we find any reference to `this.props` then we set this to true. We
    // do not need to compute this value if we found a type for
    // `propsTypeAnnotation` or `constructorPropsTypeAnnotation`.
    let usesPropsSomewhere = false;
    // If we find any reference to `this.state` then we set this to true. We
    // do not need to compute this value if we found a type for
    // `stateTypeAnnotation`.
    let usesStateSomewhere = false;
    // Look at all our class members and remove `props`, `state`, or
    // `defaultProps` if they are only used to specify a type.
    path.node.body.body = path.node.body.body.filter(member => {
      // Do we need to check to see if props or state were used?
      const checkForPropsUsage = (
        !propsTypeAnnotation &&
        !constructorPropsTypeAnnotation &&
        !usesPropsSomewhere
      );
      const checkForStateUsage = (
        !stateTypeAnnotation &&
        !usesStateSomewhere
      );
      // Check that either props or state were used in a non-static member.
      if (
        member &&
        !member.static &&
        (checkForPropsUsage || checkForStateUsage)
      ) {
        // Get the nodes for all of the either props identifiers or state
        // identifiers.
        const nodes = j(member)
          .find(j.Identifier, {
            name: name =>
              (checkForPropsUsage && name === 'props') ||
              (checkForStateUsage && name === 'state'),
          })
          .nodes();
        // If we are checking for props usage see if we found a props
        // identifier.
        if (checkForPropsUsage) {
          usesPropsSomewhere =
            nodes.findIndex(node => node.name === 'props') !== -1;
        }
        // If we are checking for state usage see if we found a state
        // identifier.
        if (checkForStateUsage) {
          usesStateSomewhere =
            nodes.findIndex(node => node.name === 'state') !== -1;
        }
      }

      if (
        // Class properties like defaultProps, props, and state.
        member.type === 'ClassProperty' &&
        member.key.type === 'Identifier'
      ) {
        if (member.static && member.key.name === 'defaultProps') {
          defaultPropsTypeAnnotation = member.typeAnnotation
            ? member.typeAnnotation.typeAnnotation
            : fallbackDefaultPropsTypeAnnotation;
          return !!member.value;
        } else if (!member.static && member.key.name === 'props') {
          propsTypeAnnotation = member.typeAnnotation
            ? member.typeAnnotation.typeAnnotation
            : fallbackPropsTypeAnnotation;
          return !!member.value;
        } else if (!member.static && member.key.name === 'state') {
          stateTypeAnnotation = member.typeAnnotation
            ? member.typeAnnotation.typeAnnotation
            : fallbackStateTypeAnnotation;
          return !!member.value;
        } else {
          return true;
        }
      } else if (
        // The constructor method.
        member.type === 'MethodDefinition' &&
        member.key.type === 'Identifier' &&
        member.key.name === 'constructor' &&
        member.value &&
        member.value.type === 'FunctionExpression' &&
        member.value.params[0]
      ) {
        constructorPropsTypeAnnotation = member.value.params[0].typeAnnotation
          ? member.value.params[0].typeAnnotation.typeAnnotation
          : fallbackPropsTypeAnnotation;
        return true;
      } else if (
        // Lifecycle methods.
        member.type === 'MethodDefinition' &&
        member.key.type === 'Identifier' &&
        LIFECYCLE_METHODS.has(member.key.name) &&
        member.value &&
        member.value.type === 'FunctionExpression'
      ) {
        const paramNames = LIFECYCLE_METHODS.get(member.key.name);
        // For all of the parameters...
        for (let i = 0; i < member.value.params.length; i++) {
          const param = member.value.params[i];
          const paramName = paramNames[i];
          // If we have any excuse to stop iterating over the lifecycle method
          // params then take it.
          if (!param || !paramName || param.type === 'RestElement') {
            return true;
          }
          // Get the type annotation. (Or the fallback.)
          const typeAnnotation =
            param.typeAnnotation &&
            param.typeAnnotation.type === 'TypeAnnotation' &&
            param.typeAnnotation.typeAnnotation;
          // If this is a props param and we don't yet have a lifecycle type
          // annotation.
          if (paramName === 'props' && !propsLifecycleTypeAnnotation) {
            propsLifecycleTypeAnnotation =
              typeAnnotation ||
              fallbackPropsTypeAnnotation;
          }
          // If this is a state param and we don't yet have a lifecycle type
          // annotation.
          if (paramName === 'state' && !stateLifecycleTypeAnnotation) {
            stateLifecycleTypeAnnotation =
              typeAnnotation ||
              fallbackStateTypeAnnotation;
          }
        }
        return true;
      } else {
        return true;
      }
    });
    // Set the super type parameters to the class.
    path.node.superTypeParameters = j.typeParameterInstantiation([
      defaultPropsTypeAnnotation,
      // Use the props type annotation if we have it. If not try using the
      // constructor props annotation. Otherwise, if we found any mention to
      // props use the any type. If not then use an empty object type.
      propsTypeAnnotation ||
        constructorPropsTypeAnnotation ||
        propsLifecycleTypeAnnotation ||
        (usesPropsSomewhere
          ? fallbackPropsTypeAnnotation
          : j.objectTypeAnnotation([])),
      // The state type annotation may be null. If it is then it will be
      // filtered out at the end. If state was used somewhere then we need the
      // fallback type annotation.
      stateTypeAnnotation ||
        stateLifecycleTypeAnnotation ||
        (usesStateSomewhere ? fallbackStateTypeAnnotation : null),
    ].filter(Boolean));
  }
};
