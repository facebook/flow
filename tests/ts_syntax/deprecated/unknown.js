type T = mixed; // ERROR

const x: mixed = 1; // ERROR

const uknown: number = 1; // OK

{
    try {}
    catch (e: mixed) {} // ERROR
}
