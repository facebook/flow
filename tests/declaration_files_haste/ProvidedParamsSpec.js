import type Immutable from 'immutable';

export type Spec = {
  description: ?string,
  id: ?string,
  ownerId: string,
  tags: Immutable.List<string>,
};

export const defaults: Partial<Spec> = {};
