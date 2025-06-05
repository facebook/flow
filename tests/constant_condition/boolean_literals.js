{
    if (true) {} // OK
    if (false) {} // OK

    let x;
    x = true ? 5 : 6; // OK
    x = false ? 5 : 6; // OK

    true && 5; // OK
    false && 5; // OK

    true || 5; // OK
    false || 5; // OK

    true ?? 5; // OK
    false ?? 5; // OK
}
