interface I_Function_this {
    +x : (this : I_Function_this) => void
}

interface I_Method_this {
    x(this : I_Method_this) : void
}

interface I_Function_mixed {
    +x : (this : unknown) => void
}

interface I_Method_mixed {
    x(this : unknown) : void
}

interface I_Function_empty {
    +x : (this : empty) => void
}

/* ----------------- */

declare class C_Function_this {
    +x : (this : C_Function_this) => void
}

declare class C_Method_this {
    x(this : C_Method_this) : void
}

declare class C_Function_mixed {
    +x : (this : unknown) => void
}

declare class C_Method_mixed {
    x(this : unknown) : void
}

declare class C_Function_empty {
    +x : (this : empty) => void
}

declare class C_Method_empty {
    +x : (this : empty) => void
}


declare var cft : C_Function_this;
declare var cmt : C_Method_this;
declare var cfm : C_Function_mixed;
declare var cmm : C_Method_mixed;
declare var cfe : C_Function_empty;

cft as I_Function_this; // error, contravariant
cft as I_Method_this; // error, contravariant
cft as I_Function_mixed; // error, contravariant
cft as I_Method_mixed; // error, contravariant
cft as I_Function_empty; // ok

cmt as I_Function_this; // error, unbound method
cmt as I_Method_this; // ok
cmt as I_Function_mixed; // error, unbound method
cmt as I_Method_mixed; // ok
cmt as I_Function_empty; // error, unbound method

cfm as I_Function_this; // ok
cfm as I_Method_this; // ok
cfm as I_Function_mixed; // ok
cfm as I_Method_mixed; // ok
cfm as I_Function_empty; // ok

cmm as I_Function_this; // error, unbound method
cmm as I_Method_this; // ok
cmm as I_Function_mixed; // error, unbound method
cmm as I_Method_mixed; // ok
cmm as I_Function_empty; // error, unbound method

cfe as I_Function_this; // error, contravariant
cfe as I_Method_this; // error, contravariant
cfe as I_Function_mixed; // error, contravariant
cfe as I_Method_mixed; // error, contravariant
cfe as I_Function_empty; // ok
