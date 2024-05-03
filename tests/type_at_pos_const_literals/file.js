declare const o: {p: number};
declare const d: {[x: string]: string};
declare const m: {m(): void};

export const obj1 = {f: 1, m() {}} as const;
//           ^
export const obj2 = {...{f: 1, m() {}}} as const;
//           ^
export const obj3 = {f: o} as const;
//           ^
export const obj4 = {...{f: {...{g: 1}}}} as const;
//           ^
export const obj5 = {...o} as const;
//           ^
export const obj6 = {...d} as const;
//           ^
export const obj7 = {...m} as const;
//           ^

declare const a: [1, 2, 3];
declare const b: Array<number>;
declare const n: number;

export const arr1 = [1, 2, 3] as const;
//           ^
export const arr2 = [n] as const;
//           ^
const arr3 = [...[1, 2, 3]] as const;
//    ^
const arr4 = [...a] as const;
//    ^
const arr5 = [...b] as const;
//    ^
export const arr6 = [{f: 1}] as const;
//           ^
const arr7 = [...[1, ...[2, 3]]] as const;
//    ^
export const arr9 = [] as const;
//           ^
