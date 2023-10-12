declare interface TestInterface {
  // flowlint unsafe-getters-setters:off
  get foo(): string;
  set foo(value: number): void;
  // flowlint unsafe-getters-setters:error
}
