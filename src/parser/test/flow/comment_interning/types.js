type Test1 = /* 1.1 L generic */ Foo /* 1.2 T generic */ . /* 1.3 L generic */ bar /* 1.4 T generic */;

type Test2</* 2.1 L variance */+T> = T;

type Test3 = /* 3.1 L nullable */ ? /* 3.2 L generic */ T /* 3.3 T generic */;

type Test4 = /* 4.1 L typeof */ typeof /* 4.2 L generic */ T /* 4.3 T generic */;

type Test5 = /* 5.1 L tuple */ [ /* 5.2 L generic */ T /* 5.3 T generic */ ] /* 5.4 T tuple */;

type Test6 = /* 6.1 L generic */ T /* 6.2 T generic */ [] /* 6.3 T array */;
