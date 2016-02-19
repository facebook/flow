// Strawman: revised definition of $jsx (alternatively, React.Element).
// Using bounded poly to specify a constraint on a type parameter, and
// existentials to elide type arguments.
type _ReactElement<D,P,Config:$Diff<P,D>,C:ReactComponent<D,P,any>> = ReactElement<Config>;
type $jsx<C> = _ReactElement<*,*,*,C>;

// type ReactComponent2<C:ReactComponent> = C
// type ReactClass2<C:ReactComponent> = Class<C>
