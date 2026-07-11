/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {
  Disposable,
  FragmentType,
  GraphQLTaggedNode,
  MutationParameters,
  OperationType,
  PayloadError,
} from 'relay-runtime';

// Opaque type for a preloaded query reference.
export opaque type PreloadedQuery<out TQuery extends OperationType>: Readonly<{
  variables: TQuery['variables'],
  ...
}> = Readonly<{
  variables: TQuery['variables'],
  __id: string,
  ...
}>;

export type UseMutationConfig<TMutation extends MutationParameters> = {
  variables: TMutation['variables'],
  onCompleted?: ?(
    response: TMutation['response'],
    errors: ?ReadonlyArray<PayloadError>,
  ) => void,
  onError?: ?(error: Error) => void,
  ...
};

declare export function graphql(
  strings: ReadonlyArray<string>,
): GraphQLTaggedNode;

declare export hook usePreloadedQuery<TQuery extends OperationType>(
  query: GraphQLTaggedNode,
  queryRef: PreloadedQuery<TQuery>,
): TQuery['response'];

// TData is inferred from the key's $data property (populated by relay-compiler).
declare export hook useFragment<
  TFragmentType extends FragmentType,
  TKey extends Readonly<{
    $fragmentSpreads: TFragmentType,
    $data?: unknown,
    ...
  }>,
>(
  fragment: GraphQLTaggedNode,
  key: TKey,
): NonNullable<TKey['$data']>;

declare export hook useMutation<TMutation extends MutationParameters>(
  mutation: GraphQLTaggedNode,
): [
  commit: (config: UseMutationConfig<TMutation>) => Disposable,
  isPending: boolean,
];

declare export hook useLazyLoadQuery<TQuery extends OperationType>(
  query: GraphQLTaggedNode,
  variables: TQuery['variables'],
): TQuery['response'];
