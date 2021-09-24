// @flow

(import.meta: mixed); // OK
(import.meta.url: ?string); // OK

(import.meta: 1); // Error
import.meta.xxx; // Error
