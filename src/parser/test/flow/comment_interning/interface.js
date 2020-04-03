/* 1.1 L interface */ interface /* 1.2 L id */ name /* 1.3 T id */ {} /* 1.4 T obj */

interface name /* 2.1 T id */ <T> /* 2.2 T tparams */ {}

interface name /* 3.1 T id */ extends /* 3.2 L id */ super1 /* 3.3 T id */, /* 3.4 L id */ super2 /* 3.5 T id */ {}

{
  interface name {} /* 4.1 T obj */
  /* 4.2 L interface */ interface name {} /* 4.3 T obj */
}
