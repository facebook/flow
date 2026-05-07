//@flow
type Disjoint = {t: 'a'}  | {t: 'b'};
declare const obj: {o: Disjoint};
const y: Disjoint = {...obj.o}; // Ok!
