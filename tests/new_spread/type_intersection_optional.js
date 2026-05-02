declare class T {}
declare class U {}

declare var o1: {...{p:T, ...}&{p:U, ...}, ...};
o1 as {p?:T&U, ...}; // ok

declare var o2: {...{p?:T, ...}&{p:U, ...}, ...};
o2 as {p?:T&U, ...}; // ok

declare var o3: {...{p:T, ...}&{p?:U, ...}, ...};
o3 as {p?:T&U, ...}; // ok

declare var o4: {...{p?:T, ...}&{p?:U, ...}, ...};
o4 as {p?:T&U, ...}; // ok

declare var o5: {...{p:T}&{p:U, ...}, ...};
o5 as {p:T&U, ...}; // ok

declare var o6: {...{p?:T}&{p:U, ...}, ...};
o6 as {p:T&U, ...}; // ok

declare var o7: {...{p:T}&{p?:U, ...}, ...};
o7 as {p:T&U, ...}; // ok

declare var o8: {...{p?:T}&{p?:U, ...}, ...};
o8 as {p?:T&U, ...}; // ok

declare var o9: {...{p:T, ...}&{p:U}, ...};
o9 as {p:T&U, ...}; // ok

declare var o10: {...{p?:T, ...}&{p:U}, ...};
o10 as {p:T&U, ...}; // ok

declare var o11: {...{p:T, ...}&{p?:U}, ...};
o11 as {p:T&U, ...}; // ok

declare var o12: {...{p?:T, ...}&{p?:U}, ...};
o12 as {p?:T&U, ...}; // ok

declare var o13: {...{p:T}&{p:U}};
o13 as {p:T&U}; // ok

declare var o14: {...{p?:T}&{p:U}};
o14 as {p:T&U}; // ok

declare var o15: {...{p:T}&{p?:U}};
o15 as {p:T&U}; // ok

declare var o16: {...{p?:T}&{p?:U}};
o16 as {p?:T&U}; // ok

declare var o17: {...{p:T, ...}&{q:U, ...}, ...};
o17 as {p?:T,q?:U, ...}; // ok

declare var o18: {...{p?:T, ...}&{q:U, ...}, ...};
o18 as {p?:T,q?:U, ...}; // ok

declare var o19: {...{p:T, ...}&{q?:U, ...}, ...};
o19 as {p?:T,q?:U, ...}; // ok

declare var o20: {...{p?:T, ...}&{q?:U, ...}, ...};
o20 as {p?:T,q?:U, ...}; // ok

declare var o21: {...{p:T}&{q:U, ...}, ...};
o21 as {p:T,q?:U, ...}; // ok
