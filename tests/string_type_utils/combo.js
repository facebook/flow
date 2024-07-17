type CSSVarInter = StringPrefix<'var(--'> & StringSuffix<')'>;
'var(--color)' as CSSVarInter; // OK
'bar(--color)' as CSSVarInter; // ERROR
'var(--color' as CSSVarInter; // ERROR

type CSSVarRemainder = StringPrefix<'var(--', StringSuffix<')'>>;
'var(--color)' as CSSVarRemainder; // OK
'bar(--color)' as CSSVarRemainder; // ERROR
'var(--color' as CSSVarRemainder; // ERROR
