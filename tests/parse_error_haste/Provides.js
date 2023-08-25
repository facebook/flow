/**
 * Parse errors, imported, not in flow.
 * Should see a parse error in this file, but module
 * should be found in client.
 *
 * @noflow
 */
function f(s: string): string { ### // illegal token
  return s;
}

module.exports = { f: f }
