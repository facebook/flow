// @flow

(<fbt />: number);
(<fbt />: string); // Error (the libdef in this test marks fbt as number)

<fbt desc={unboundVariable}/>; // Error (missing var)
