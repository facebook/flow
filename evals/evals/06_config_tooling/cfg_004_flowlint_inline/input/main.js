// @flow

export function greeting(name: ?string): string {
  if (!name) {
    return 'Hello, stranger!';
  }
  return `Hello, ${name}!`;
}
