{
    if(!'afa') {} // ERROR -> falsy

    let a = 3
    if(a ? !'afa' : 's') {} // OK, lhs and rhs diverges

    if(a ? 'afa' : !5) {} // OK, lhs and rhs diverges

    if(a ? 'afa' : 5) {} // ERROR -> truthy
    if(a ? !'afa' : !5) {} // ERROR -> falsy

    'afa' && a; // ERROR -> truthy
    (!'afa') && a; // ERROR -> falsy

    'afa' || a; // ERROR -> truthy
    (!'afa') || a; // ERROR -> falsy

    'afa' ?? a; // ERROR -> truthy
    (!'afa') ?? a; // ERROR -> falsy
}

{
    // more nested case
    if(!!!'afa') {} // ERROR -> falsy

    let a = 3
    if(a ? !!!!!'afa' : 's') {} // OK, lhs and rhs diverges

    if(a ? 'afa' : !!!5) {} // OK, lhs and rhs diverges

    if(!(a ? !!'afa' : !!5)) {} // ERROR -> falsy
    if(a ? !!!!!!!'afa' : !!!5) {} // ERROR -> falsy

    'afa' && a; // ERROR -> truthy
    (!!!'afa') && a; // ERROR -> falsy

    'afa' || a; // ERROR -> truthy
    (!!!!!!!'afa') || a; // ERROR -> falsy

    'afa' ?? a; // ERROR -> truthy
    (!!!!!!!!!!!'afa') ?? a; // ERROR -> falsy
}
