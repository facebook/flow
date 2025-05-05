function f() {
    a!;
    f()!;
    g!();
    a!.b()!.c!;
    a?.b!.c;
    a?.b!.c!;
    (a?.b)!.c;
    a!.b()![c]!;
    a!.b!()![c]!;
    delete a!.b;
    a?.b!()![c];
    delete a.b!;

    // Errors
    a!.[b];
    a!.();
    a?.b!.()!.[c];
}
