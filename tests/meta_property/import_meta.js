// @flow

(import.meta: mixed); // OK
(import.meta.url: ?string); // OK
(import.meta.XXX: mixed); // OK

(import.meta: 1); // Error
(import.meta.XXX: 1); // Error
