try {
} catch (e) { // OK
  (e: empty); // ERROR - is `mixed`
  (e: mixed); // OK - is `mixed`
}
