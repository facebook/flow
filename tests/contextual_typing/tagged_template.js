// Tests to make sure that we pass hints to tagged templates

declare function poly<T>(x: TaggedTemplateLiteralArray): T;

const x: string = poly`hello world!`; // NO ERROR
