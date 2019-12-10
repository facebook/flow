/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

'use strict';

module.exports = (j: any) => {
  /**
   * Has this path imported the react module and given a name it a name? If
   * React is imported like:
   *
   * ```
   * import {Component} from 'react';
   * ```
   *
   * Then it does not count.
   */
  function getImportedReactName(path: any): ?string {
    // All of the modules we want to treat as React.
    const REACT_MODULES = new Set(['react', 'React']);
    // Find the first require for React that does not destructure React but
    // instead gives it a name.
    const reactRequire = path
      .findVariableDeclarators()
      .filter(
        j.filters.VariableDeclarator.requiresModule(Array.from(REACT_MODULES)),
      )
      .nodes()
      .find(node => node.id && node.id.type === 'Identifier');
    // If we found a require for React then return the name.
    if (reactRequire) {
      return reactRequire.id.name;
    }
    // Get all of the import declarations that import React.
    const reactImports = path
      .find(j.ImportDeclaration, {
        type: 'ImportDeclaration',
        source: {
          type: 'Literal',
          value: value => REACT_MODULES.has(value),
        },
      })
      .nodes();
    // For all of the React imports...
    for (let i = 0; i < reactImports.length; i++) {
      const reactImport = reactImports[i];
      // ...and for all of each import's specifiers...
      for (let j = 0; j < reactImport.specifiers.length; j++) {
        const specifier = reactImport.specifiers[j];
        // ...check to see if it is either a default specifier or a namespace
        // specifier. If it is either and it has a local name then return that
        // local name.
        if (
          (specifier.type === 'ImportDefaultSpecifier' ||
            specifier.type === 'ImportNamespaceSpecifier') &&
          specifier.local &&
          specifier.local.type === 'Identifier'
        ) {
          return specifier.local.name;
        }
      }
    }
    // Otherwise we can't find anything and should return null.
    return null;
  }

  /**
   * Gets a pattern that can be used with jscodeshift that matches value nodes
   * like `React.Component`. If React was not imported then null will be
   * returned.
   */
  function getImportedComponentClassPattern(path: any): Function | null {
    // All of the places where we want to treat as React.
    const REACT_MODULES = new Set([
      'React',
      'react',
      'react/addons',
      'react-native',
    ]);
    // The names of the exports from `REACT_MODULES` that are components
    // classes.
    const COMPONENT_CLASSES = new Set(['Component', 'PureComponent']);
    // Do we require React?
    const requiresReact =
      path
        .findVariableDeclarators()
        .filter(
          j.filters.VariableDeclarator.requiresModule(
            Array.from(REACT_MODULES),
          ),
        )
        .size() > 0;
    // Do we import React?
    const importsReact =
      path
        .find(j.ImportDeclaration, {
          type: 'ImportDeclaration',
          source: {
            type: 'Literal',
            value: value => REACT_MODULES.has(value),
          },
        })
        .size() > 0;
    // If we neither require React or import React then we cannot get a
    // component class pattern.
    if (!requiresReact && !importsReact) {
      return null;
    }
    // We want to match two different patterns so we jump straight to a
    // function.
    //
    // For now we use a simple implementation where we assume Component and/or
    // PureComponent has not been renamed. In the future if it becomes a problem
    // then we should check to see if Component and/or PureComponent were
    // renamed.
    return (node: any) =>
      node &&
      // Matches: `Component`.
      ((node.type === 'Identifier' && COMPONENT_CLASSES.has(node.name)) ||
        // Matches: `React.Component`.
        (node.type === 'MemberExpression' &&
          node.object.type === 'Identifier' &&
          node.object.name === 'React' &&
          node.property.type === 'Identifier' &&
          COMPONENT_CLASSES.has(node.property.name)) ||
        // Matches: `React.Component` but in the type position.
        (node.type === 'QualifiedTypeIdentifier' &&
          node.qualification.type === 'Identifier' &&
          node.qualification.name === 'React' &&
          node.id.type === 'Identifier' &&
          COMPONENT_CLASSES.has(node.id.name)));
  }

  /**
   * Checks for a very specific pattern:
   *
   * ```
   * const React = require('react');
   * const {Element} = React;
   * ```
   *
   * or:
   *
   * ```
   * import type {Element} from 'react';
   * ```
   *
   * At this moment we do not care about generalizing these patterns!
   */
  function hasDestructuredElement(path: any, reactName?: ?string) {
    // All of the places where we want to treat as React.
    const REACT_MODULES = new Set(['React', 'react', 'react-native']);
    reactName = reactName || getImportedReactName(path);
    return (
      path
        .find(j.ImportDeclaration, {
          specifiers: specifiers =>
            specifiers.find(
              specifier =>
                specifier &&
                specifier.type === 'ImportSpecifier' &&
                specifier.imported &&
                specifier.imported.type === 'Identifier' &&
                specifier.imported.name === 'Element' &&
                specifier.local &&
                specifier.local.type === 'Identifier' &&
                specifier.local.name === 'Element',
            ),
          source: {
            type: 'Literal',
            value: value => REACT_MODULES.has(value),
          },
        })
        .size() > 0 ||
      path
        .find(j.VariableDeclarator, {
          init: {
            type: 'Identifier',
            name: reactName,
          },
          id: {
            type: 'ObjectPattern',
            properties: properties =>
              !!properties.find(
                property =>
                  property &&
                  !property.method &&
                  property.shorthand &&
                  property.key &&
                  property.key.type === 'Identifier' &&
                  property.key.name === 'Element' &&
                  property.value &&
                  property.value.type === 'Identifier' &&
                  property.value.name === 'Element',
              ),
          },
        })
        .size() > 0
    );
  }

  return {
    getImportedReactName,
    getImportedComponentClassPattern,
    hasDestructuredElement,
  };
};
