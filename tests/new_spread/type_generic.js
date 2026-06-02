declare function spread<A,B>(a: A, b: B): {...A, ...B};
spread({p:0},{q:0}) as {readonly p:number,readonly q:number}; // ok
spread({p:0},{q:0}) as {readonly p:empty,readonly q:empty}; // number ~> empty (x2)
