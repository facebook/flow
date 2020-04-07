/* 1.1 L decl class */ declare /* 1.2 L decl class */ class Class {} /* 1.3 T obj type */

declare class /* 2.1 L id */ Class /* 2.2 T id */ <T> /* 2.3 L tparams */ {}

declare class Class /* 3.1 T id */ extends /* 3.2 L id */ Super /* 3.3 T id */ {}

declare class Class /* 4.1 T id */ implements /* 4.2 L id */ Interface /* 4.3 T id */ {}

declare class Class /* 5.1 T id */ mixins /* 5.2 L id */ Mixin /* 5.3 T id */ {}

{
  /* 6.1 L decl */ declare class Class {} /* 6.2 T obj type */
  /* 6.3 L decl */ declare class Class {}
  /* 6.4 T obj type */
}
