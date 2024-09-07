type T = {p:string}|{p:number};
type U = $ObjMap<T,<V>(V) => V>;
({p:0}: T); // ok
({p:0}: U); // ok
