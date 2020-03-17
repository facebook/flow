type Test1 = {/* 1.1 L variance */ + /* 1.2 L id */ x /* 1.3 T id */: 1};

type Test2 = {/* 2.1 L variance */ +[string]: string };

type Test3 = {/* 3.1 L spread */ ... /* 3.2 L obj */ {} /* 3.3 T obj */};
