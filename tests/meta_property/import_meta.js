import.meta as unknown; // OK
import.meta.url as ?string; // OK
import.meta.XXX as unknown; // OK

import.meta as 1; // Error
import.meta.XXX as 1; // Error
