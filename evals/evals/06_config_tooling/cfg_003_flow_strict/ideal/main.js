// @flow strict

export function summarize(meta: {label: string}, count: ?number): string {
  let result = '';
  if (count != null) {
    result += `count=${count} `;
  }
  return result + String(meta.label);
}
