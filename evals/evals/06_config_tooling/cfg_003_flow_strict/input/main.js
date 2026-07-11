// @flow strict

export function summarize(meta: Object, count: ?number): string {
  let result = '';
  if (count) {
    result += `count=${count} `;
  }
  return result + String(meta.label);
}
