type Test1 = /* 1.1 L generic */ Foo /* 1.2 T generic */ . /* 1.3 L generic */ bar /* 1.4 T generic */;

type Test2</* 2.1 L variance */ + /* 2.2 L tparam */ T /* 2.3 T tparam */ : /* 2.4 L bound */ Bound /* 2.5 T bound */ = /* 2.5 L default */ Default /* 2.6 T default */ > = T;

type Test3 = /* 3.1 L nullable */ ? /* 3.2 L generic */ T /* 3.3 T generic */;

type Test4 = /* 4.1 L typeof */ typeof /* 4.2 L generic */ T /* 4.3 T generic */;

type Test5 = /* 5.1 L tuple */ [ /* 5.2 L generic */ T /* 5.3 T generic */ ] /* 5.4 T tuple */;

type Test6 = /* 6.1 L generic */ T /* 6.2 T generic */ [] /* 6.3 T array */;

type Test7 = /* 7.1 L string lit */ 'foo' /* 7.2 T string lit */;

type Test8 = /* 8.1 L num lit */ 1 /* 8.2 T num lit */;

type Test9 = /* 9.1 L bigint lit */ 1n /* 9.2 T bigint lit */;
