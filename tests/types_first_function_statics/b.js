const a = require('./a');

a as () => void;
a.x as number;
a.x as string;
a.other as number;

const poly_a = require('./poly_a');

poly_a as () => void;
poly_a as <T>() => void;
poly_a.x as number;
poly_a.x as string;
poly_a.other as number;
