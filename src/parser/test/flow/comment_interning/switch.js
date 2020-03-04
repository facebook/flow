switch /* 1.1 Not attached to switch */ (1) /* 1.2 Not attached to switch */ { /* 1.3 Not attached to switch */
    default:
        1;
/* 1.4 Not attached to switch */ }

/* 2.1 Leading on switch */ switch (1) {
    default:
        1;
}

switch (1) {
    default:
        1;
} /* 3.1 Trailing on switch */

/* 4.1 Leading on switch */ switch (1) {
    default:
        1;
} /* 4.2 Trailing on switch */
