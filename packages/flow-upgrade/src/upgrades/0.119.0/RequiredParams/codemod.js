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

function createContext(filePath, scope, j) {
  return {
    filePath,
    j,
    scope,
    stack: [{}],
  };
}

function addInScope(type, context) {
  const scope = context.stack[context.stack.length - 1];
  switch(type.type) {
    case 'TypeParameter':
      scope[type.name] = type;
      break;
    case 'TypeAlias':
      scope[type.id.name] = type;
      break;
    default:
      console.log(type);
  }
}

function findInScope(name, context) {
  for (let i = context.stack.length - 1; i >= 0; i--) {
    if (context.stack[i][name]) {
      return {
        context,
        type: context.stack[i][name],
      };
    }
  }

  let scope = context.scope;
  while(scope) {
    scope.lookupType(name);
    if (scope.types[name]) {
      const n = scope.types[name].length;
      const parentPath = scope.types[name][n-1].parentPath;
      return {
        type: parentPath.value,
        context: createContext(context.filePath, parentPath.scope, context.j),
      };
    }
    scope = scope.parent;
  }

  return null;
}

function enterScope(context, typeParameters) {
  context.stack.push({});
  if (typeParameters) {
    typeParameters.params.forEach(
      typeParam => {
        addInScope(typeParam, context);
      }
    )
  }
}

function exitScope(context) {
  context.stack.pop();
}

function isTypeOptional(typeAnnotation, context) {
  switch(typeAnnotation.type) {
    case 'NullableTypeAnnotation':
      return true;
    case 'IntersectionTypeAnnotation':
      return typeAnnotation.types.every(t => isTypeOptional(t, context));
    case 'UnionTypeAnnotation':
      return typeAnnotation.types.some(t => isTypeOptional(t, context));
    case 'VoidTypeAnnotation':
    case 'MixedTypeAnnotation':
    case 'AnyTypeAnnotation':
      return true;
    case 'FunctionTypeAnnotation':
      handleFunctionTypeAnnotation(typeAnnotation, context);
      return false;
    case 'GenericTypeAnnotation': {
      const sourceType = findInScope(typeAnnotation.id.name, context);
      // TODO: considering not bound as optional should be an option
      return sourceType ? isTypeOptional(sourceType.type, sourceType.context) : false;
    }
    case 'TypeParameter': {
      const { bound } = typeAnnotation;
      if (!bound) return true;
      return isTypeOptional(bound.typeAnnotation, context);
    }
    case 'NumberTypeAnnotation':
    case 'StringTypeAnnotation':
    case 'NullLiteralTypeAnnotation':
      return false;
    case 'ObjectTypeAnnotation':
      handleObjectTypeAnnotation(typeAnnotation, context);
      return false;
    case 'TypeAlias': {
      return isTypeOptional(typeAnnotation.right, context);
    }
    default:
      console.log(typeAnnotation);
      return false;
  }
}

function handleObjectTypeAnnotation(type, context) {
  if (type.callProperties) {
    type.callProperties.forEach((callProperty) => {
      handleFunctionTypeAnnotation(callProperty.value, context);
    })
  }
  if (type.properties) {
    type.properties.forEach((prop) => {
      if (prop.value.type === 'FunctionTypeAnnotation') {
        handleFunctionTypeAnnotation(prop.value, context);
      }
    });
  }
}

function handleFunctionTypeAnnotation(type, context) {
  enterScope(context, type.typeParameters);
  handleFunctionTypeParam(type.params, context);
  type = context.j.functionTypeAnnotation(
    type.params, type.returnType, type.rest, type.typeParameters,
  );
  exitScope(context);
}

function handleFunctionTypeParam(params, context) {
  const n = params.length;
  for(let i = n-1; i >= 0; i--) {
    const param = params[i];
    if (param.type === 'RestElement') continue;
    if (!param.typeAnnotation) return;
    if (param.optional) continue;

    const { typeAnnotation } = param;

    if (isTypeOptional(typeAnnotation, context)) {
      param.optional = true;

      if (!param.name) {
        // ?number => string ~~> (_?: ?number, _?: void) => string
        param.name = context.j.identifier('_');
      }
    }
  }
}

function handleParams(params, context) {
  const n = params.length;
  for(let i = n-1; i >= 0; i--) {
    const param = params[i];
    if (param.type === 'RestElement') continue;
    if (!param.typeAnnotation) return;
    if (param.optional) continue;

    const { typeAnnotation } = param.typeAnnotation

    if (isTypeOptional(typeAnnotation, context)) {
      param.optional = true;
    }
  }
}

function handleFunction(path, context) {
  const { params, typeParameters } = path.node;

  switch (path.parentPath.value.type) {
    case 'MethodDefinition':
    case 'ClassProperty':
      return;
    default:
  }

  enterScope(context, typeParameters);
  handleParams(params, context);
  exitScope(context);
}

function handleClassBody(body, context) {
  body.body.forEach((statement) => {
    if (statement.type !== 'MethodDefinition' && statement.type !== 'ClassProperty') {
      return;
    }

    const statementValue = statement.value;

    if (!['ArrowFunctionExpression', 'FunctionExpression'].includes(statementValue.type)) {
      return;
    }

    const { params, typeParameters } = statementValue;

    enterScope(context, typeParameters);
    handleParams(params, context);
    exitScope(context);
  });
}

function handleClass(path, context) {
  const { body, typeParameters } = path.node;

  enterScope(context, typeParameters);
  handleClassBody(body, context);
  exitScope(context);
}

function handleInterface(path, context) {
  const { body, typeParameters } = path.node;

  enterScope(context, typeParameters);
  handleObjectTypeAnnotation(body, context);
  exitScope(context);
}

function handleTypeAlias(path, context, j) {
  const { right, typeParameters } = path.node;

  if (right.type === 'FunctionTypeAnnotation') {
    enterScope(context, typeParameters);
    handleFunctionTypeAnnotation(right, context);
    exitScope(context);
    return;
  }

  if (right.type === 'ObjectTypeAnnotation') {
    enterScope(context, typeParameters);
    handleObjectTypeAnnotation(right, context);
    exitScope(context);
    return;
  }

}

function handleDeclareFunction(path, context, j) {
  const { id: { typeAnnotation: { typeAnnotation } } } = path.node;

  if (typeAnnotation.type !== 'FunctionTypeAnnotation') {
    return;
  }
  handleFunctionTypeAnnotation(typeAnnotation, context);
}

module.exports = (j: any, root: any, fileInfo: any) => {
  // root.find(j.ImportDeclaration);
  // root.find(j.ImportSpecifier);

  root
    .find(j.TypeAlias)
    .forEach(function(p) { handleTypeAlias(p, createContext(fileInfo.path, this.scope, j) )});

  root
    .find(j.FunctionDeclaration)
    .forEach(function(p) { handleFunction(p, createContext(fileInfo.path, this.scope, j) )});

  root
    .find(j.FunctionExpression)
    .forEach(function(p) { handleFunction(p, createContext(fileInfo.path, this.scope, j) )});

  root
    .find(j.ArrowFunctionExpression)
    .forEach(function(p) { handleFunction(p, createContext(fileInfo.path, this.scope, j) )});

  root
    .find(j.DeclareFunction)
    .forEach(function(p) { handleDeclareFunction(p, createContext(fileInfo.path, this.scope, j) )});

  root
    .find(j.DeclareVariable)
    .forEach(function(p) { handleDeclareFunction(p, createContext(fileInfo.path, this.scope, j) )});

  root.find(j.ClassDeclaration)
    .forEach(function(p) { handleClass(p, createContext(fileInfo.path, this.scope, j) )});

  root.find(j.InterfaceDeclaration)
    .forEach(function(p) { handleInterface(p, createContext(fileInfo.path, this.scope, j) )});

  return root.toSource({tabWidth: 2});
};
