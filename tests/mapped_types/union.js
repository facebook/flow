type T = {p:string}|{p:number};
type IdentityMap<O> = {[K in keyof O]: O[K]};
type U = IdentityMap<T>;
({p:0}: T); // ok
({p:0}: U); // ok
({p:'0'}: T); // ok
({p:'0'}: U); // ok
({p:false}: T); // error
({p:false}: U); // error
