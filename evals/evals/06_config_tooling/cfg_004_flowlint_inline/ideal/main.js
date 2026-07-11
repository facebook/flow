// @flow

export function greeting(name: ?string): string {
  // flowlint-next-line sketchy-null-string:off
  if (!name) {
    return 'Hello, stranger!';
  }
  return `Hello, ${name}!`;
}
