//@flow

let obj = {
  /**
   * identifier property of an object literal
   */
  a : 1,
  /**
   * literal property of an object literal
   */
  'b' : 2,
  /**
   * computed property of an object literal
   */
  ['c'] : 3,
  /**
   * annotated getter property of an object literal
   */
  get d(): number { return 4; },
  /**
   * unannotated getter property of an object literal (TODO)
   */
  get e()      {},
  /**
   * method property of an object literal
   */
  f() {},
  /**
   * async method property of an object literal
   */
  async g() {},
  /**
   * member h
   * @explorer-desc
   *  this is h
   *  it's a member
   * @explorer-ignore
   * @explorer-title h the member
   */
  h : 5,
}

obj.x
//  ^
