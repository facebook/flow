/**
 * @format
 */

'use strict';

// All the event names that we need to add <> to.
const SYNTHETIC_EVENT_NAMES = new Set([
  'SyntheticEvent',
  'SyntheticAnimationEvent',
  'SyntheticClipboardEvent',
  'SyntheticCompositionEvent',
  'SyntheticInputEvent',
  'SyntheticUIEvent',
  'SyntheticFocusEvent',
  'SyntheticKeyboardEvent',
  'SyntheticMouseEvent',
  'SyntheticDragEvent',
  'SyntheticWheelEvent',
  'SyntheticTouchEvent',
  'SyntheticTransitionEvent',
]);

/**
 * This codemod alters, removes, and replaces various React utilities that
 * had API changes when upgrading to this version. This includes the following
 * list:
 *
 * - `React$Node`.
 * - `React$Element`.
 * - `ReactClass`.
 * - `ReactComponent`.
 * - The return type of React component render.
 */
module.exports = (j, root) => {
  const recast = require('recast');
  const ReactUtils = require('../../../codemods/ReactUtils')(j);
  const reactName = ReactUtils.getImportedReactName(root);
  const componentPattern = ReactUtils.getImportedComponentClassPattern(root);
  const hasElement = ReactUtils.hasDestructuredElement(root, reactName);

  // There is a bug in recast so we can't use `root.find()`. Instead we need to
  // visit with recast manually so we can visit the nodes it misses.
  root.paths().forEach(path => {
    recast.visit(path, {
      // Here is where we do all of our transformations.
      visitGenericTypeAnnotation: visitGenericTypeAnnotation,

      // We want to replace default React imports with a namespace import.
      visitImportDeclaration: visitImportDeclaration,

      // recast has a bug where it won't visit `superTypeParameters` on class
      // declarations and expressions. Fix that here.
      visitClassDeclaration: visitClass,
      visitClassExpression: visitClass,
    });
  });

  function visitClass(path) {
    // Continue traversing the path. This is the default behavior.
    this.traverse(path);
    // There is a bug in recast which means we end up not traversing
    // `superTypeParameters`! So handle that that bug by visiting
    // `superTypeParameters` here.
    this.visitWithoutReset(path.get('superTypeParameters'));
  }

  function visitImportDeclaration(path) {
    const node = path.node;

    // import React from 'react'; ==> import * as React from 'react';
    if (
      node.importKind === 'value' &&
      node.source &&
      node.source.type === 'Literal' &&
      node.source.value === 'react' &&
      node.specifiers.length === 1 &&
      node.specifiers[0].type === 'ImportDefaultSpecifier'
    ) {
      node.specifiers[0] = j.importNamespaceSpecifier(node.specifiers[0].local);
    }

    // Continue traversal if we do not want to apply a transformation to
    // this node.
    this.traverse(path);
  }

  function visitGenericTypeAnnotation(path) {
    const node = path.node;

    // React$Node<any> ==> React.Node
    if (
      node.id.type === 'Identifier' &&
      (node.id.name === 'React$Node' || node.id.name === 'ReactNode')
    ) {
      // React.Node no longer has type parameters.
      node.typeParameters = null;
      // Use the exported type from the React module. If it does not exist then
      // use React$Node.
      node.id = reactName
        ? j.qualifiedTypeIdentifier(
            j.identifier(reactName),
            j.identifier('Node'),
          )
        : j.identifier('React$Node');
      // We have handled this node. No need to continue traversing.
      return false;
    }

    // React$Element<Config> ==> React.Element<React.ComponentType<Props>>
    if (
      // React$Element or ReactElement
      (node.id.type === 'Identifier' &&
        (node.id.name === 'React$Element' ||
          node.id.name === 'ReactElement')) ||
      // React.Element
      (node.id.type === 'QualifiedTypeIdentifier' &&
        node.id.qualification &&
        node.id.qualification.type === 'Identifier' &&
        node.id.qualification.name === reactName &&
        node.id.id &&
        node.id.id.type === 'Identifier' &&
        node.id.id.name === 'Element') ||
      // const {Element} = React;
      // Element
      (hasElement &&
        node.id.type === 'Identifier' &&
        node.id.name === 'Element')
    ) {
      // If we have no type parameters then add an empty type parameter
      // instantiation.
      if (!node.typeParameters) {
        node.typeParameters = j.typeParameterInstantiation([]);
      }
      // If we have a first type parameter...
      if (node.typeParameters.params[0]) {
        const firstTypeParam = node.typeParameters.params[0];
        // ...and that type parameter is not `any`, `mixed`, or `*`...
        if (
          firstTypeParam &&
          firstTypeParam.type !== 'AnyTypeAnnotation' &&
          firstTypeParam.type !== 'ExistsTypeAnnotation'
        ) {
          // ...then wrap the type parameter with `React.ComponentType<Props>`.
          node.typeParameters.params[0] = j.genericTypeAnnotation(
            // Get the `React.ComponentType` type name, or use the global.
            reactName
              ? j.qualifiedTypeIdentifier(
                  j.identifier(reactName),
                  j.identifier('ComponentType'),
                )
              : j.identifier('React$ComponentType'),
            // Put the type param we are wrapping as the first and only
            // type argument.
            j.typeParameterInstantiation([firstTypeParam]),
          );
        }
      }
      // If we do not have a first type parameter, or our type parameter is `*`
      // then we want to use `any` instead. This is because using `*` will often
      // cause errors that are unrelated to whether or not the code is correct.
      // Almost all of the time when a user writes `React.Element<*>` they meant
      // `React.Element<any>`. They just felt bad about writing `any`.
      if (
        node.typeParameters.params.length === 0 ||
        node.typeParameters.params[0].type === 'ExistsTypeAnnotation'
      ) {
        node.typeParameters = j.typeParameterInstantiation([
          j.anyTypeAnnotation(),
        ]);
      }
      // Update the type identifier to be `React.Element` or the
      // equivalent global.
      node.id = reactName
        ? j.qualifiedTypeIdentifier(
            j.identifier(reactName),
            j.identifier('Element'),
          )
        : j.identifier('React$Element');
      // We have handled this node. No need to continue traversing.
      return false;
    }

    // ReactClass<Config> ==> React.ComponentType<Props>
    if (
      node.id.type === 'Identifier' &&
      node.id.name === 'ReactClass' &&
      node.typeParameters &&
      node.typeParameters.type === 'TypeParameterInstantiation'
    ) {
      // Replace ReactClass with the new type.
      return j.genericTypeAnnotation(
        // Get the component type name. Either as a qualified name from our
        // React import or using the global.
        reactName
          ? j.qualifiedTypeIdentifier(
              j.identifier(reactName),
              j.identifier('ComponentType'),
            )
          : j.identifier('React$ComponentType'),
        // Keep the type parameters we had previously.
        node.typeParameters,
      );
    }

    // React$Component<Defaults, Props, State> ==> React.Component<Props, State>
    if (
      ((node.id.type === 'Identifier' &&
        (node.id.name === 'ReactComponent' ||
          node.id.name === 'React$Component')) ||
        (componentPattern && componentPattern(node.id))) &&
      node.typeParameters &&
      node.typeParameters.type === 'TypeParameterInstantiation'
    ) {
      // Remove the default props type argument.
      if (node.typeParameters.params.length >= 3) {
        node.typeParameters.params.shift();
      }
      // If the user has destructured React and used Component or PureComponent
      // directly then we should not update the identifier of this node.
      if (
        node.id.type === 'Identifier' &&
        (node.id.name === 'Component' || node.id.name === 'PureComponent')
      ) {
        return false;
      }
      // Replace the identifier with React.Component
      node.id = reactName
        ? j.qualifiedTypeIdentifier(
            j.identifier(reactName),
            j.identifier('Component'),
          )
        : j.identifier('React$Component');
      // We have handled this node. No need to continue traversing.
      return false;
    }

    // SyntheticEvent ==> SyntheticEvent<>
    if (
      node.id.type === 'Identifier' &&
      SYNTHETIC_EVENT_NAMES.has(node.id.name)
    ) {
      node.typeParameters = j.typeParameterInstantiation([]);
      return false;
    }

    // Continue traversal if we do not want to apply a transformation to
    // this node.
    this.traverse(path);
  }

  // render(): React.Element<any> ==> render(): React.Node
  if (componentPattern) {
    root
      .find(j.ClassDeclaration, {
        superClass: componentPattern,
        body: {type: 'ClassBody'},
      })
      .forEach(handleReactClassPath);

    root
      .find(j.ClassExpression, {
        superClass: componentPattern,
        body: {type: 'ClassBody'},
      })
      .forEach(handleReactClassPath);
  }

  function handleReactClassPath(path) {
    // For every member in the class's body...
    path.node.body.body.forEach(method => {
      // If this member is the component's render method and it has a
      // return type...
      if (
        method.type === 'MethodDefinition' &&
        method.key &&
        method.key.type === 'Identifier' &&
        method.key.name === 'render' &&
        method.value &&
        method.value.type === 'FunctionExpression' &&
        method.value.returnType &&
        method.value.returnType.type === 'TypeAnnotation'
      ) {
        const returnType = method.value.returnType;
        // If this is a return type from render that people commonly use then
        // replace it with the new recommendation. React.Node
        if (
          isRenderReturnReactElement(returnType.typeAnnotation) ||
          (returnType.typeAnnotation.type === 'NullableTypeAnnotation' &&
            isRenderReturnReactElement(
              returnType.typeAnnotation.typeAnnotation,
            ))
        ) {
          returnType.typeAnnotation = reactName
            ? j.qualifiedTypeIdentifier(
                j.identifier(reactName),
                j.identifier('Node'),
              )
            : j.identifier('React$Node');
        }
      }
    });
  }

  function isRenderReturnReactElement(node) {
    // Is the node React.Element<any>, React.Element<mixed>,
    // or React.Element<*>?
    return (
      reactName &&
      node &&
      node.type === 'GenericTypeAnnotation' &&
      node.id.type === 'QualifiedTypeIdentifier' &&
      node.id.qualification.type === 'Identifier' &&
      node.id.qualification.name === reactName &&
      node.id.id.type === 'Identifier' &&
      node.id.id.name === 'Element' &&
      node.typeParameters &&
      node.typeParameters.type === 'TypeParameterInstantiation' &&
      (node.typeParameters.params.length === 0 ||
        (node.typeParameters.params[0] &&
          (node.typeParameters.params[0].type === 'AnyTypeAnnotation' ||
            node.typeParameters.params[0].type === 'ExistsTypeAnnotation')))
    );
  }

  return true;
};
