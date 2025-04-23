function test<T>(v: T) {
    Object.assign({}, v); // ok, but really should error
}
