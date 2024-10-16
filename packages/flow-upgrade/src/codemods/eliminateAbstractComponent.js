/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {CodemodModule} from '../Types';
import type {GenericTypeAnnotation, TypeAnnotationType} from 'hermes-estree';
import type {MaybeDetachedNode} from 'hermes-transform';

import {codemod} from '../Types';
import {t} from 'hermes-transform';

function eliminateAbstractComponent(
  node: GenericTypeAnnotation,
): MaybeDetachedNode<TypeAnnotationType> | null {
  const {id: maybeQualifiedIdentifier, typeParameters} = node;

  // `React.AbstractComponent<Props>` -> `React.ComponentType<Props>`
  // `React.AbstractComponent<Props, mixed>` -> `React.ComponentType<Props>`
  if (
    maybeQualifiedIdentifier.type === 'QualifiedTypeIdentifier' &&
    maybeQualifiedIdentifier.qualification.name === 'React' &&
    maybeQualifiedIdentifier.id.name === 'AbstractComponent' &&
    typeParameters != null
  ) {
    const [config, instance, renders] = typeParameters.params;

    if (renders != null) {
      return null;
    }

    if (instance == null || instance.type === 'MixedTypeAnnotation') {
      return t.GenericTypeAnnotation({
        id: t.QualifiedTypeIdentifier({
          qualification: t.Identifier({name: 'React'}),
          id: t.Identifier({name: 'ComponentType'}),
        }),
        typeParameters: t.TypeParameterInstantiation({params: [config]}),
      });
    }

    return null;
  }

  // React.ElementConfig<React.AbstractComponent<Props, ...> -> Props
  if (
    maybeQualifiedIdentifier.type === 'QualifiedTypeIdentifier' &&
    maybeQualifiedIdentifier.qualification.name === 'React' &&
    maybeQualifiedIdentifier.id.name === 'ElementConfig' &&
    typeParameters != null &&
    typeParameters.params.length === 1 &&
    typeParameters.params[0].type === 'GenericTypeAnnotation' &&
    typeParameters.params[0].id.type === 'QualifiedTypeIdentifier' &&
    typeParameters.params[0].id.qualification.name === 'React' &&
    typeParameters.params[0].id.id.name === 'AbstractComponent' &&
    typeParameters.params[0].typeParameters != null &&
    typeParameters.params[0].typeParameters.params.length >= 1
  ) {
    return typeParameters.params[0].typeParameters.params[0];
  }

  // React.ElementConfig<React.AbstractComponent<Props, Instance, ...> -> Instance
  if (
    maybeQualifiedIdentifier.type === 'QualifiedTypeIdentifier' &&
    maybeQualifiedIdentifier.qualification.name === 'React' &&
    maybeQualifiedIdentifier.id.name === 'ElementRef' &&
    typeParameters != null &&
    typeParameters.params.length === 1 &&
    typeParameters.params[0].type === 'GenericTypeAnnotation' &&
    typeParameters.params[0].id.type === 'QualifiedTypeIdentifier' &&
    typeParameters.params[0].id.qualification.name === 'React' &&
    typeParameters.params[0].id.id.name === 'AbstractComponent' &&
    typeParameters.params[0].typeParameters != null &&
    typeParameters.params[0].typeParameters.params.length >= 2
  ) {
    return typeParameters.params[0].typeParameters.params[1];
  }

  // React.ElementConfig<React.AbstractComponent<Props> -> mixed
  if (
    maybeQualifiedIdentifier.type === 'QualifiedTypeIdentifier' &&
    maybeQualifiedIdentifier.qualification.name === 'React' &&
    maybeQualifiedIdentifier.id.name === 'ElementRef' &&
    typeParameters != null &&
    typeParameters.params.length === 1 &&
    typeParameters.params[0].type === 'GenericTypeAnnotation' &&
    typeParameters.params[0].id.type === 'QualifiedTypeIdentifier' &&
    typeParameters.params[0].id.qualification.name === 'React' &&
    typeParameters.params[0].id.id.name === 'AbstractComponent' &&
    typeParameters.params[0].typeParameters != null &&
    typeParameters.params[0].typeParameters.params.length === 1
  ) {
    return t.MixedTypeAnnotation({});
  }

  return null;
}

export default codemod({
  title: 'Eliminate `React.AbstractComponent<...>`',
  describe: `
- \`React.AbstractComponent<Props>\` -> \`React.ComponentType<Props>\`
- \`React.ElementConfig<React.AbstractComponent<Props, Instance>>\` -> \`Props\`
- \`React.ElementRef<React.AbstractComponent<Props, Instance>>\` -> \`Instance\`
- \`React.ElementRef<React.AbstractComponent<Props>\` -> \`mixed\``,
  transform: context => {
    return {
      GenericTypeAnnotation(node) {
        const replacement = eliminateAbstractComponent(node);
        if (replacement) {
          context.replaceNode(node, replacement, {
            keepComments: true,
          });
        }
      },
    };
  },
}) as CodemodModule;
