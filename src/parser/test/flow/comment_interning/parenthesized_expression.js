var i = 0;

/* Nested leading 1 */ ( /* Nested leading 2 */ i /* Nested trailing 1 */ ) /* Nested trailing 2 */;

/* Leading array */ ([]) /* Trailing array */;

/* Leading assign */ (i = 1) /* Trailing assign */;

/* Leading binary */ (1 + 2) /* Trailing binary */;

/* Leading class */ (class C {}) /* Trailing class */;

/* Leading identifier */ (i) /* Trailing identifier */;

/* Leading literal */ (1) /* Trailing literal */;

/* Leading new */ (new C()) /* Trailing new */;

/* Leading object */ ({}) /* Trailing object */;

/* Leading unary */ (+1) /* Trailing unary */;

/* Leading update */ (i++) /* Trailing update */;

function* generator() {
    /* Leading yield */ (yield 1) /* Trailing yield */
}
