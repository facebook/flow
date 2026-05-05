// No annotation
try {
} catch (e) { // OK
  e as empty; // OK - is `any`
  e as unknown; // OK - is `any`
}

// `any` annotation
try {
} catch (e: any) { // OK
  e as empty; // OK - is `any`
  e as unknown; // OK - is `any`
}

// `mixed` annotation
try {
} catch (e: unknown) { // OK
  e as empty; // ERROR - is `mixed`
  e as unknown; // OK - is `mixed`

  if (e instanceof Error) { // OK
    e as Error; // OK
  } else {
    throw e;
  }
}

// `unknown` annotation
try {
} catch (e: unknown) { // OK
  e as empty; // ERROR - is `unknown`
  e as unknown; // OK - is `unknown`

  if (e instanceof Error) { // OK
    e as Error; // OK
  } else {
    throw e;
  }
}

// Invalid annotation
try {
} catch (e: Error) { // ERROR
  e as empty; // OK - is `any`
  e as unknown; // OK - is `any`
}
