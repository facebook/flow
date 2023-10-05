export component Foo() { return null }
export type RendersFoo = renders React$Element<typeof Foo>;
export type RendersMaybeFoo = renders? Foo;
export type RendersStarFoo = renders* Foo;

export component MaybeNumber() renders? number { return null } 
export component StarNumber() renders* number { return null } 
