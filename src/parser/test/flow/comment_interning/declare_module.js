/* 1.1 L decl mod */ declare /* 1.2 L decl mod */ module /* 1.3 L id */ Foo /* 1.4 T id */ {} /* 1.5 T block */

{
  /* 2.1 L decl */ declare module Foo {} /* 2.2 T block */
  /* 2.3 L decl */ declare module Foo {}
  /* 2.4 T block */
}
