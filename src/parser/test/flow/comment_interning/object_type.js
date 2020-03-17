type Test1 = /* 1.1 L obj */ {} /* 1.2 T obj */;

type Test2 = {/* 2.1 L variance */ + /* 2.2 L id */ x /* 2.3 T id */: 1};

type Test3 = {/* 3.1 L variance */ +[string]: string };

type Test4 = {/* 4.1 L spread */ ... /* 4.2 L obj */ {} /* 4.3 T obj */};
