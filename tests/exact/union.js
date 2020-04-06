({p:'A'}: $Exact<{p:'A'}|{p:'B'}>); // ok
({p:'B'}: $Exact<{p:'A'}|{p:'B'}>); // ok
({p:'A',q:0}: $Exact<{p:'A'}|{p:'B'}>); // error: extra prop
({p:'C'}: $Exact<{p:'A'}|{p:'B'}>); // error: no match
