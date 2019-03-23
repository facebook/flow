// @flow

let tests = [
  // disallow calling TemplateStringsArray interface
  function() {
    new TemplateStringsArray();
  },

  // list of args
  function() {
    function tag(quasis: TemplateStringsArray, x: number, y: number) {
      (quasis.raw: $ReadOnlyArray<string>);
    }
    tag`foo${1}bar${2}`; // ok
    tag`foo${"bad"}bar${2}`; // error: string !~> number
  },

  // wrong arity
  function() {
    function tag(quasis: TemplateStringsArray, number) {
      (quasis.raw: $ReadOnlyArray<string>);
    }
    tag`foo${1}bar${2}`; // error: expected 2 args, got 3
  },

  // rest expr
  function() {
    function tag(quasis: TemplateStringsArray, ...exprs: Array<number>) {
      (quasis.raw: $ReadOnlyArray<string>);
    }
    tag`foo${1}`; // ok
    tag`foo${"bad"}`; // error: string !~> number
  }
];
