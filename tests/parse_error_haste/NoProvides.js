/**
 * Parse errors, imported, not in flow, no provides module.
 * Should see a parse error in this file, and module
 * not found in client.
 * @noflow
 */
function f(s:string):string { ### // illegal token
  return s;
}

module.exports = { f: f }
