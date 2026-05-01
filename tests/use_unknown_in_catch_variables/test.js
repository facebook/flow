try {
} catch (e) { // OK
  e as empty; // ERROR - is `unknown`
  e as unknown; // OK - is `unknown`
}
