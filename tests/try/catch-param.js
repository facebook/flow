// @flow

// No annotation
try {
} catch (e) { // OK
  (e: empty); // OK - is `any`
  (e: mixed); // OK - is `any`
}

// `any` annotation
try {
} catch (e: any) { // OK
  (e: empty); // OK - is `any`
  (e: mixed); // OK - is `any`
}

// `mixed` annotation
try {
} catch (e: mixed) { // OK
  (e: empty); // ERROR - is `mixed`
  (e: mixed); // OK - is `mixed`

  if (e instanceof Error) { // OK
    (e: Error); // OK
  } else {
    throw e;
  }
}

// Invalid annotation
try {
} catch (e: Error) { // ERROR
  (e: empty); // OK - is `any`
  (e: mixed); // OK - is `any`
}
