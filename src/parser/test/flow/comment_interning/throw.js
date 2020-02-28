function test() {
    // 1.1 Not a leading comment
    var F = function(){};
    // 1.2 Leading comment
    /* 1.3 Leading comment */ throw /* 1.4 Not a trailing comment */ new Error('Error') /* 1.5 Not trailing */;
}
