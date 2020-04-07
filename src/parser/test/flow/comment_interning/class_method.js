class C {
  /* 1.1 L id */ method /* 1.2 T id */ <T> /* 1.3 L params */ () /* 1.4 T params */ {} /* 1.5 T block */

  /* 2.1 L meth */ static /* 2.2 L meth */ async /* 2.3 L meth */ * /* 2.4 L id */ method(){}

  /* 3.1 L meth */ get /* 3.2 L id */ getter /* 3.3 T id */ () /* 3.4 T params */ {} /* 3.5 T block */
  /* 4.1 L meth */ set /* 4.2 L id */ setter /* 4.3 T id */ (x) /* 4.4 T params */ {} /* 4.5 T block */

  /* 5.1 L meth */ static /* 5.2 L meth */ get /* 5.3 L id */ getter() {}
  /* 6.1 L meth */ static /* 6.2 L meth */ set /* 6.3 L id */ setter(x) {}
}

class C { method(){} /* 7.1 L id */ method(){} /* 7.2 T block */ }

class C {
  /* 8.1 L id */ method(){} /* 8.2 T block */
  /* 8.3 L id */ method(){} /* 8.4 T block */
  /* 8.5 T block */
}
