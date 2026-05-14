// `experimental.deprecated_variance_sigils=true` deprecates `+` and `-` variance
// sigils for files outside the configured excludes — keywords like
// `readonly` / `writeonly` / `in` / `out` are unaffected by this flag.

type CovObj = { +p: number }; // error: + deprecated
type ContraObj = { -p: number }; // error: - deprecated

class A<+T> {} // error: + deprecated
class B<-T> {} // error: - deprecated

type CovIdx = { +[k: string]: number }; // error: + deprecated
type ContraIdx = { -[k: string]: number }; // error: - deprecated

class CField {
  +covField: number; // error: + deprecated
  -contraField: number; // error: - deprecated
}

type Alias<+T> = T; // error: + deprecated
type AliasNeg<-T> = (T) => void; // error: - deprecated

interface ICov<+T> {} // error: + deprecated
interface IContra<-T> {} // error: - deprecated

interface IProp {
  +covProp: number; // error: + deprecated
  -contraProp: number; // error: - deprecated
}

// Keywords still pass when allow_variance_keywords=true.
type W = { writeonly p: number }; // ok
class CIn<in T> {} // ok
class COut<out T> {} // ok
