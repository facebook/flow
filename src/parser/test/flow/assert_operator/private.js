class C {
    #p = 42;
    f(other: C) {
        other!.#p;
    }
}
