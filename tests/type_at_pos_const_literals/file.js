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
