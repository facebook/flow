(*****************************************************************************)
(* An IST for the HHVM bytecode *)
(*****************************************************************************)

open SharedCollections

type ident = Id of string
type name = string
type label = string

type class_kind =
  | KClass (** Plain old class. *)
  | KInterface
  | KFinalClass
  | KTrait
  | KAbstractFinal

type type_ =
  | Tany
  | Tvoid
  | Tapply of applied_type
  | Ttuple of type_ list
  | Tfun of type_ list * type_
  | Tparam of name * type_ option
  | Tthis of type_
  | Tinst of type_
  | Tinter of applied_type list

and applied_type = name * type_ list

type attribute =
  | AttrStatic
  | AttrAbstract
  | AttrPublic
  | AttrPrivate
  | AttrMayusevv

type program = {
    class_defs      : class_def UMap.t;
    fun_defs        : fun_def UMap.t;
  }

and class_def = {
    cla_name        : ident;
    cla_kind        : class_kind;
    cla_implements  : ident list;
    cla_extends     : ident list;
    cla_properties  : (string * attribute list) list;
    cla_uses        : type_ list UMap.t;
    cla_met_use     : (name * name list) UMap.t;
    cla_types       : (string * ident * type_) UMap.t;
    cla_methods     : method_def UMap.t;
    cla_constructor : method_def;
    cla_lambdas     : lambda_def UMap.t;
  }

and type_hint = {
    tyh_user        : type_;
    tyh_hhvm        : (ident * hint_flag list) option;
  }

and hint_flag =
  | HintVar
  | HintExtended
  | HintSoft

and method_def = {
    met_name        : string;
    met_attrs       : attribute list;
    met_return      : type_hint;
    met_params      : parameter list;
    met_locals      : SSet.t;
    met_async       : bool;
    met_blocks      : blocks;
  }

and fun_def = {
    fun_name        : ident;
    fun_attrs       : attribute list;
    fun_return      : type_hint;
    fun_params      : parameter list;
    fun_locals      : SSet.t;
    fun_async       : bool;
    fun_blocks      : blocks;
    fun_lambdas     : lambda_def UMap.t;
  }

and parameter = string * type_hint

and lambda_def = {
    lam_name        : ident;
    lam_kind        : lambda_kind;
    lam_params      : string list;
    lam_uses        : SSet.t;
    lam_locals      : SSet.t;
    lam_blocks      : blocks;
  }

and lambda_kind =
  | LamAsyncBlock
  | LamNormal of bool (* async *)

and blocks = block list

and block =
  | BSimple of simple_block
  | BTryCatch of blocks * catch_block list

and catch_block = ident * label * blocks
and simple_block = label option * instr list * jump

and jump =
  | JNext
  | JJmp of label
  | JJmpZ of label
  | JJmpNZ of label
  | JRetC
  | JThrow
  | JFatal of fatal

and fatal =
  | FatalRuntimeOmitFrame

and instr =
  | IAGetC
  | IAdd
  | IAwait
  | IBareThis
  | IBaseC of int
  | IBaseH
  | IBaseL of string
  | ICGetL of string
  | ICUGetL of string
  | ICatch
  | ICheckThis
  | IClsCns of string
  | IConcat
  | ICreateCl of int * ident
  | IDim of member
  | IDiv
  | IDouble of float
  | IDup
  | IEq
  | IFCall of int
  | IFPassCE of int
  | IFPushClsMethod of int
  | IFPushClsMethodD of int * string * ident
  | IFPushCtor of int
  | IFPushCtorD of int * ident
  | IFPushFunc of int
  | IFPushFuncD of int * ident
  | IFPushObjMethodD of int * string
  | IFalse
  | IGt
  | IGte
  | IInstanceOfD of ident
  | IInt of Int64.t
  | IIsTypeCNull
  | ILateBoundCls
  | ILt
  | ILte
  | IMod
  | IMul
  | INameA
  | INeq
  | INewPackedArray of int
  | INot
  | INull
  | IPopC
  | IPopR
  | IQueryM of int * member
  | ISetL of string
  | ISetM of int * member
  | IString of string
  | IStringId of ident
  | ISub
  | ITrue
  | IUnboxR

and member =
  | MembField of string
  | MembIndex of int
