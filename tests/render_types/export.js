export component Foo() { return null }
export type RendersFoo = renders ExactReactElement_DEPRECATED<typeof Foo>;
export type RendersMaybeFoo = renders? Foo;
export type RendersStarFoo = renders* Foo;

export component MaybeFoo() renders? Foo { return null }
export component StarFoo() renders* Foo { return null }
