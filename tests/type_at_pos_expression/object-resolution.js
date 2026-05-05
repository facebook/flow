// @flow

type O = {p: number};
declare var ro: Readonly<O>;
ro.p as number;

type O1 = {p: number, ...};
type O2 = {p: number; q: string, ...};
declare var diff: Omit<O2, keyof O1>;
diff.q as string;

declare var spread: { ...O2, ... };
if (spread.q) spread.q as string;
