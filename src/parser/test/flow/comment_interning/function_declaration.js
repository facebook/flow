/* 1.1 L func */ async /* 1.2 L func */ function /* 1.3 L func */ * /* 1.4 L id */ test /* 1.5 T id */ () /* 1.6 T params */ {} /* 1.7 T block */

export default function /* 2.1 L params */ () /* 2.2 T params */ {}

function /* 3.1 L id */ name /* 3.2 T id */ <T> /* 3.3 L params */ () {}

function name() /* 4.1 T params */ : /* 4.2 L any */ any /* 4.3 T any */ {}

function name(): /* 5.1 L pred */ %checks /* 5.2 T pred */ {}

function name(): /* 6.1 L pred */ %checks /* 6.2 L pred */ (true) /* 6.3 T pred */ {}

function name(): boolean /* 7.1 T boolean */ %checks /* 7.2 T pred */ {}
