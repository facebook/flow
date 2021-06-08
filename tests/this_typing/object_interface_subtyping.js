// @flow

interface I_Function_this {
    +x : (this : I_Function_this) => void
}

interface I_Method_this {
    x(this : I_Method_this) : void
}

interface I_Function_mixed {
    +x : (this : mixed) => void
}

interface I_Method_mixed {
    x(this : mixed) : void
}

interface I_Function_empty {
    +x : (this : empty) => void
}

/* ----------------- */

type C_Function_this = {
    +x : (this : C_Function_this) => void
}

type C_Method_this = {
    x(this : C_Method_this) : void
}

type C_Function_mixed = {
    +x : (this : mixed) => void
}

type C_Method_mixed = {
    x(this : mixed) : void
}

type C_Function_empty = {
    +x : (this : empty) => void
}

type C_Method_empty = {
    +x : (this : empty) => void
}


declare var cft : C_Function_this;
declare var cmt : C_Method_this;
declare var cfm : C_Function_mixed;
declare var cmm : C_Method_mixed;
declare var cfe : C_Function_empty;
declare var cme : C_Method_empty;

(cft : I_Function_this); // error, contravariant
(cft : I_Method_this); // error, contravariant
(cft : I_Function_mixed); // error, contravariant
(cft : I_Method_mixed); // error, contravariant
(cft : I_Function_empty);

(cmt : I_Function_this);  // error, contravariant
(cmt : I_Method_this);  // error, contravariant
(cmt : I_Function_mixed);  // error, contravariant
(cmt : I_Method_mixed);  // error, contravariant
(cmt : I_Function_empty);

(cfm : I_Function_this);
(cfm : I_Method_this);
(cfm : I_Function_mixed);
(cfm : I_Method_mixed);
(cfm : I_Function_empty);

(cmm : I_Function_this);
(cmm : I_Method_this);
(cmm : I_Function_mixed);
(cmm : I_Method_mixed);
(cmm : I_Function_empty);

(cfe : I_Function_this);  // error, contravariant
(cfe : I_Method_this); // error, contravariant
(cfe : I_Function_mixed); // error, contravariant
(cfe : I_Method_mixed); // error, contravariant
(cfe : I_Function_empty);

(cme : I_Function_this); // error, contravariant
(cme : I_Method_this); // error, contravariant
(cme : I_Function_mixed); // error, contravariant
(cme : I_Method_mixed); // error, contravariant
(cme : I_Function_empty);
