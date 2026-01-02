var a = { p: 0 };
(a.q ? a.q : "foo"); // a is marked as refined, because a is refined to empty
(a.q: unknown); // An error here

var b = { p: 0 };
if (b.q) {} else {};
(b.q: unknown); // is an error here
