/* 1.1 L JSX element */ <div>test</div> /* 1.2 T JSX element */;

</* 2.1 L JSX id */ div /* 2.2 T JSX id */>test</ /* 2.3 L JSX id */ div /* 2.4 T JSX id */>;

</* 3.1 L JSX id */ Member /* 3.2 T JSX id */ . /* 3.3 L JSX id */ div /* 3.4 JSX id */ />;

</* 4.1 L JSX id */ Namespace /* 4.2 T JSX id */ : /* 4.3 L JSX id */ div /* 4.4 JSX id */ />;

<div /* 5.1 L JSX id */ name /* 5.2 JSX id */ />;

<div /* 6.1 L JSX id */ name /* 6.2 T JSX id */="test" />;

<div name=/* 7.1 L JSX expr */ {/* 7.2 L num */ 1 /* 7.3 T num */} /* 7.4 T JSX expr */ />;

<div>{/* 8.1 L JSX spread */ ... /* 8.2 L obj */ {} /* 8.3 T obj */}</div>;
