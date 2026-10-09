mergedFunction(1) as number;
mergedFunction.member(1) as number;
mergedFunction.member('text') as string;
mergedFunction.member(1) as string; // ERROR

const result: mergedFunction.NumberResult = 1;
const badResult: mergedFunction.NumberResult = 'text'; // ERROR
