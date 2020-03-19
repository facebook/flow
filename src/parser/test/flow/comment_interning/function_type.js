type Test1 = /* 1.1 L generic */ X /* 1.2 T generic */ => /* 1.3 L generic */ Y /* 1.4 T generic */;

type Test2 = /* 2.1 L params */ ( /* 2.2 L generic */ X /* 2.3 T generic */) /* 2.4 T params */ => /* 2.5 L generic */ Y /* 2.6 T generic */;

type Test3 = <T> /* 3.1 L params */ (/* 3.2 T generic */ T /* 3.3 L generic */) /* 3.4 T params */ => T;
