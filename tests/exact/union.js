({p:'A'} as {p:'A'}|{p:'B'}); // ok
({p:'B'} as {p:'A'}|{p:'B'}); // ok
({p:'A',q:0} as {p:'A'}|{p:'B'}); // error: extra prop
({p:'C'} as {p:'A'}|{p:'B'}); // error: no match
