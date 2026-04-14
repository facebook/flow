'use strict';

const Immutable = require('immutable');

const ProvidedParamsBase = Immutable.Record({ // ERROR
  description: undefined,
  id: undefined,
  ownerId: undefined,
  tags: undefined,
});

class ProvidedParams extends ProvidedParamsBase {}

export default ProvidedParams;
