try {
} catch (e) { // OK
  (e: empty); // ERROR - is `unknown`
  (e: unknown); // OK - is `unknown`
}
