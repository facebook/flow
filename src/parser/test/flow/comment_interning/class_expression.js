/* @flow */

n = /* c01 */ class /* c02 */ ClassName /* c03 */ {} /* c04 */ ; /* c05 */

a = /* c11 */ class /* c12 */  {} /* c14 */ ; /* c15 */

(class {
  /* 2.1 L func */ async /* 2.2 L func */ * /* 2.3 L id */ method1 /* 2.4 T id */ (){} /* 2.5 T block */

  /* 3.1 L id */ method2 /* 3.2 T id */ <T> /* 3.3 L params */ () /* 3.4 L block */ {}

  get /* 4.1 L id */ getter /* 4.2 T id */ () /* 4.3 L block */ {}

  set /* 5.1 L id */ setter /* 5.2 T id */ (x) /* 5.3 L block */ {}
});
