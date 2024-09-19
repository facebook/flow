//@flow

function test(x: React$AbstractComponent<any, any, any>) {
  return;
}

function test2(x: React$AbstractComponent<{foo: string, bar?: number}, any, RendersFoo>) {
  return;
}

function test3(x: React$AbstractComponent<{foo: string, bar?: number, ...}, any, renders* Foo>) {
  return;
}

function test4(x: React$AbstractComponent<{foo: string, bar?: number, get getter(): boolean}, any, any>) { // we give up on getters
  return;
}

declare component Foo();
type RendersFoo = renders Foo;
