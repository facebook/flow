const constOne = 1;
const constNumber = 1 as number;
let letOne = 1;
function foo() {}

// okay
1 as const; // okay
"foo" as const; // okay
true as const; // okay
({}) as const; // okay
({ constOne } as const); // okay the 'as const' refers to outer object literal
[] as const; // okay
[constOne] as const; // okay the 'as const' refers to outer array literal
constOne as const; // okay constOne is const-primitive variable

// errors
(function f() {}) as const; // error function is not a literal
(class C {}) as const; // error class is not a literal
1 as const as const; // error as const is not a literal
1 + 1 as const; // error binary operation is not a literal
1 as number as const; // error cast expression is not a literal
null as const; // error null is not a supported literal
undefined as const; // error undefined is not a supported literal
foo() as const; // error function call is not a literal
constNumber as const; // error as const on non-primitive const variable
letOne as const; // error as const on non-const variable
