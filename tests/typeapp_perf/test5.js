/* This test ensures that the two array typeapps are not erroneously equated by
 * the PolyInstantiation cache. This can happen if enough distinguishing
 * information is not included in the reasons, specifically the annot location.
 *
 * If broken, this test will find an erroneous type incompatibility between
 * number and string in the call to map. */

function f(cond: boolean, as: Array<number>, bs: Array<string>) {
  const xs = cond ? as : bs;
  xs.map(x => {})
}
