declare function spread<A,B>(a: A, b: B): {...A, ...B};
spread({p:0},{q:0}) as {+p:number,+q:number}; // ok
spread({p:0},{q:0}) as {+p:empty,+q:empty}; // number ~> empty (x2)
