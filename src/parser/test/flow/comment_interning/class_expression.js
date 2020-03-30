/* @flow */

(/* 0.1 L class */ class /* 0.2 L id */ ClassName /* 0.3 T id */ <T> /* 0.4 T tparams */ {} /* 0.5 T class */); /* 0.6 T expr stmt */

(/* 1.1 L class */ class /* 1.2 L tparams */ <T> /* 1.3 T tparams */ {} /* 1.4 T class */); /* 1.5 T expr stmt */

(class {
  /* 2.1 L func */ async /* 2.2 L func */ * /* 2.3 L id */ method1 /* 2.4 T id */ (){} /* 2.5 T block */

  /* 3.1 L id */ method2 /* 3.2 T id */ <T> /* 3.3 L params */ () /* 3.4 T params */ {}

  get /* 4.1 L id */ getter /* 4.2 T id */ () /* 4.3 T params */ {}

  set /* 5.1 L id */ setter /* 5.2 T id */ (x) /* 5.3 T params */ {}
});
