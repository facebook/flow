type Test1 = /* 1.1 L obj */ {} /* 1.2 T obj */;

type Test2 = {/* 2.1 L variance */ + /* 2.2 L id */ x /* 2.3 T id */: 1};

type Test3 = {/* 3.1 L indexer */ [ /* 3.2 L type */ string /* 3.3 T type */]: /* 3.4 L type */ string /* 3.5 T type */};

type Test4 = {/* 4.1 L variance */ + /* 4.2 L indexer */ [string] /* 4.3 T indexer */ : string };

type Test5 = {/* 5.1 L spread */ ... /* 5.2 L obj */ {} /* 5.3 T obj */};

type Test6 = {/* 6.1 L islot */ [[ /* 6.2 L id */ foo /* 6.3 T id */]] /* 6.4 T islot */ : /* 6.5 L type */ string /* 6.6 T type */};

type Test7 = {/* 7.1 L params */ (/* 7.2 L generic */ X /* 7.3 T generic */) /* 7.4 T params */ : /* 7.5 L generic */ Y /* 7.6 T generic */ };

type Test8 = {/* 8.1 L tparams */ <T> /* 8.2 L params */ () /* 8.3 T params */: void};

type Test9 = {/* 9.1 L id */ method /* 9.2 T id */ <T> /* 9.3 L params */ () /* 9.4 T params */: void};

type Test10 = {/* 10.1 L islot */ [[key]] /* 10.2 L tparams */ <T> /* 10.3 L params */ () /* 10.4 T params */: void}
