{
    if (null) {} // ERROR

    let x;
    x = null ? 5 : 6; // ERROR

    null && 5; // ERROR

    null || 5; // ERROR

    null ?? 5; // ERROR
}
