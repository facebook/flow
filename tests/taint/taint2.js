/*
 *
 * @flow
 */
class A {
  f(tainted : $Tainted<string>) {
    // This *should* give a warning.
    document.location.assign(tainted);
  }
}
