/* 1.1 L alias */ type /* 1.2 L id */ Foo1 /* 1.3 T id */ <T> /* 1.4 T tparams */ = /* 1.5 L type */ boolean /* 1.6 T type */; /* 1.7 T alias */

/* 2.1 L opaque */ opaque /* 2.2 L opaque */ type /* 2.3 L id */ Foo2 /* 2.4 T id */ <T> /* 2.5 T tparams */ = /* 2.6 L type */ boolean /* 2.7 T type */; /* 2.8 T opaque */

{
  /* 3.1 L alias */ type t = any /* 3.2 T any */
  /* 3.3 L alias */ type t = any
  /* 3.4 T any */
}

{
  /* 4.1 L opaque */ opaque type T = any /* 4.2 T any */
  /* 4.3 L opaque */ opaque type T = any
  /* 4.4 T any */
}
