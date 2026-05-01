({p:'A'} as $Exact<{p:'A'}|{p:'B'}>); // ok
({p:'B'} as $Exact<{p:'A'}|{p:'B'}>); // ok
({p:'A',q:0} as $Exact<{p:'A'}|{p:'B'}>); // error: extra prop
({p:'C'} as $Exact<{p:'A'}|{p:'B'}>); // error: no match
