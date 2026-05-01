const a = require('./a');

a as () => void;
a.g as string;
a.g as number;
a.other as string;

const poly_a = require('./poly_a');

poly_a as () => void;
poly_a as <T>() => void;
poly_a.g as string;
poly_a.g as number;
poly_a.other as string;
