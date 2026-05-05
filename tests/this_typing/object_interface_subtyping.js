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

type C_Function_this = {
    +x : (this : C_Function_this) => void
, ...}

type C_Method_this = {
    x(this : C_Method_this) : void
, ...}

type C_Function_mixed = {
    +x : (this : unknown) => void
, ...}

type C_Method_mixed = {
    x(this : unknown) : void
, ...}

type C_Function_empty = {
    +x : (this : empty) => void
, ...}

type C_Method_empty = {
    +x : (this : empty) => void
, ...}


declare var cft : C_Function_this;
declare var cmt : C_Method_this;
declare var cfm : C_Function_mixed;
declare var cmm : C_Method_mixed;
declare var cfe : C_Function_empty;
declare var cme : C_Method_empty;

cft as I_Function_this; // error, contravariant
cft as I_Method_this; // error, contravariant
cft as I_Function_mixed; // error, contravariant
cft as I_Method_mixed; // error, contravariant
cft as I_Function_empty;

cmt as I_Function_this;  // error, contravariant
cmt as I_Method_this;  // error, contravariant
cmt as I_Function_mixed;  // error, contravariant
cmt as I_Method_mixed;  // error, contravariant
cmt as I_Function_empty;

cfm as I_Function_this;
cfm as I_Method_this;
cfm as I_Function_mixed;
cfm as I_Method_mixed;
cfm as I_Function_empty;

cmm as I_Function_this;
cmm as I_Method_this;
cmm as I_Function_mixed;
cmm as I_Method_mixed;
cmm as I_Function_empty;

cfe as I_Function_this;  // error, contravariant
cfe as I_Method_this; // error, contravariant
cfe as I_Function_mixed; // error, contravariant
cfe as I_Method_mixed; // error, contravariant
cfe as I_Function_empty;

cme as I_Function_this; // error, contravariant
cme as I_Method_this; // error, contravariant
cme as I_Function_mixed; // error, contravariant
cme as I_Method_mixed; // error, contravariant
cme as I_Function_empty;
