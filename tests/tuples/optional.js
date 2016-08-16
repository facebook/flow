// @flow

([0]: [number, ?string]); // OK, since second element marked optional

([]: [?number, string]); // error, since second element is not marked optional
