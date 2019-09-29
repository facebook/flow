function t0() {
        t1(); // a leading comment
        // another leading comment
        return 42; // should not be a trailing comment
}

function t1() {
	return /* trailing comment */;
}
