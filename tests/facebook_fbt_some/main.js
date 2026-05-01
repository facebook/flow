<fbt /> as number;
<fbt /> as string; // Error (the libdef in this test marks fbt as number)

<fbt desc={unboundVariable}/>; // Error (missing var)
