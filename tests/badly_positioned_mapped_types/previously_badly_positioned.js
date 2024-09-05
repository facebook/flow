type C = <H>(H) => {[K in keyof H]: H[K]};
declare var c: C;
c('string')(); // error: string is not an object
