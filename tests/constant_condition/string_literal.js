{
    if ('') {} // ERROR
    if ('\'') {} // ERROR
    if ('a') {} // ERROR
    if ('hfudiashfkdsnfsa') {} // ERROR

    let x;
    x = '' ? 5 : 6; // ERROR
    x = '\'' ? 5 : 6; // ERROR
    x = 'a' ? 5 : 6; // ERROR
    x = 'fdsgfdgsfdga' ? 5 : 6; // ERROR

    '' && 5; // ERROR
    '\'' && 5; // ERROR
    'a' && 5; // ERROR
    'fdsgfdgsfdga' && 5; // ERROR

    '' || 5; // ERROR
    '\'' || 5; // ERROR
    'a' || 5; // ERROR
    'fdsgfdgsfdga' || 5; // ERROR

    '' ?? 5; // ERROR
    '\'' ?? 5; // ERROR
    'a' ?? 5; // ERROR
    'fdsgfdgsfdga' ?? 5; // ERROR
}
