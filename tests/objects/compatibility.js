let tests = [
  function(x: { x: { foo: string, ... }, ... }, y: { x: { bar: number, ... }, ... }) {
    x = y; // 2 errors: `foo` not found in y.x; `bar` not found in x.x
  },

  function(x: { foo: string, ... }, y: { foo: number, ... }) {
    x = y; // 2 errors: string !~> number; number !~> string
  },

  function(x: { x: { foo: string, ... }, ... }, y: { x: { foo: number, ... }, ... }) {
    x = y; // 2 errors: string !~> number; number !~> string
  },

  function(x: { readonly foo: string, ... }, y: { readonly foo: number, ... }) {
    x = y; // 1 error: number !~> string
  },

  function(x: { x: { readonly foo: string, ... }, ... }, y: { x: { readonly foo: number, ... }, ... }) {
    x = y; // 2 errors: string !~> number; number !~> string
  },

  function(x: { writeonly foo: string, ... }, y: { writeonly foo: number, ... }) {
    x = y; // 1 error: string !~> number
  },

  function(x: { x: { writeonly foo: string, ... }, ... }, y: { x: { writeonly foo: number, ... }, ... }) {
    x = y; // 2 errors: string !~> number; number !~> string
  },
];
