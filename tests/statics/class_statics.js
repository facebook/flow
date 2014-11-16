class C {
  static f(x:number) { }
}

C.g = function(x:string) { C.f(x); };
C.g(0);
