import.meta as mixed; // OK
import.meta.url as ?string; // OK
import.meta.XXX as mixed; // OK

import.meta as 1; // Error
import.meta.XXX as 1; // Error
