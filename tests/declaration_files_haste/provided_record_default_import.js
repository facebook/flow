import type Immutable from 'immutable';
import type {FormProps} from './ProvidedFormTypes';

import ProvidedParams from 'ProvidedParams';
import {validateProvidedParams} from './ProvidedValidation';

declare const tags: Immutable.List<string>;

const params = new ProvidedParams({
  ownerId: '1',
  tags,
});

params.get('tags').get(0) as string | void;

declare const formProps: FormProps;

formProps.params.get('tags').get(0) as string | void;
validateProvidedParams(params).valid as boolean;
