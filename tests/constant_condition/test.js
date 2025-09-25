import React from "react";

{
  if ('') {} // ERROR
  if ('\'') {} // ERROR
  if ('a') {} // ERROR

  if('a' || 'b') {} // ERROR

  '' ? 1 : 2; // ERROR
  'a' ? 1 : 2; // ERROR
  '' || 1; // ERROR
  '' && 1; // ERROR
  'a' && 1; // ERROR
  '' ?? 1; // ERROR
  'a' ?? 1; // ERROR

  if (('a'?1:2)||3) {} // ERROR
  if (('a'?1:2)&&3) {} // ERROR
  if (('a'?1:2)??3) {} // ERROR
  if ((true?1:2)||3) {} // OK
  if ((false?1:2)||3) {} // OK

  let x = 0
  if ('a' && x) {} // ERROR
}

{
  if(true) {} // OK
  if(false) {} // OK
  true ? 1 : 2; // OK
  false || 1; // OK
}

{
  if(null) {} // ERROR
  null ? 1 : 2; // ERROR
}

{
  if (0) {} // OK
  if (1) {} // OK
  if (2) {} // ERROR

  if (0n) {} // OK
  if (1n) {} // OK
  if (2n) {} // ERROR
}

{
  if (+0) {} // OK
  if (+1) {} // OK
  if (-2) {} // ERROR

  if (+0n) {} // OK
  if (+1n) {} // OK
  if (-2n) {} // ERROR

  if (void 6) {} // ERROR
  if (typeof 'aaa') {} // ERROR
}

{
  if ('hello world'.match(/hello/)) {} // OK
  if (/hello/) {} // ERROR
}

{
  if(0 as number) {} // OK
  if(1 as number) {} // OK
  if(2 as number) {} // OK, user defined annotation takes priority
  if(3 as any) {} // OK, user defined annotation takes priority
}

{
  if([0]) {} // ERROR
  if([0,1,2]) {} // ERROR
}

{
  if(new Set<string>()) {} // ERROR
  if(new Set([0])) {} // ERROR
  if(new Set([1])) {} // ERROR
  if(new Set([2])) {} // ERROR
}

{
  let x;
  if(x=0) {} // OK
  if(x=1) {} // OK
  if(x=2) {} // ERROR
  x=0 ? 1 : 2; // OK
  x=1 ? 1 : 2; // OK
  x=2 ? 1 : 2; // ERROR
}

{
  let x=0;
  let f = () => 0;
  if ((x,0)) {} // OK
  if ((x,2,1)) {} // OK
  if ((x,x,x,2)) {} // ERROR
  if ((0,0,()=>0)) {} // ERROR
  if((0,0,f)) {} // ERROR: f is always truthy
  if((0,0,f())) {} // OK
  if((0,1,2,'a'?f():3)) {} // ERROR
  if((0,1,2,
    'a'?f():3, // ERROR
    2) // ERROR
    ) {}
  if((0,1,2,
    'a'?f():3, // ERROR
    1) // OK
  ) {}
}

{
  if({
    a: 1,
    b: 2,
  }){} // ERROR
}

{
  if(function(num: number){return num%2===0}) {} // ERROR
  if(function(num: number){return num%2===0}(2)) {} // OK

  if((a:number) => a%2===0) {} // ERROR
  if(((a:number) => a%2===0)(6)) {} // OK
}

{
  if (`I'm TemplateLiteral`) {} // ERROR
}

{
  if (<div>true</div>) {} // ERROR
  if (<div>false</div>) {} // ERROR
  if (<>true</>) {} // ERROR
  if (<>false</>) {} // ERROR
}

{
  if(class A {}){} // ERROR
  function* numbers() {
    for (let i = 0; i < 10; i++) {
      yield i; // OK
      if (yield i) {} // ERROR
    }
  }
}

{
  if ((('a'?2:3)?4:5)) {} // ERROR
  if ((true?2:3)?4:5) {} // ERROR
  if ((false?2:3)?4:5) {} // ERROR
  if ((('a'?2:3)?4:5)?6:7) {} // ERROR
  if (((true?2:3)?4:5)?6:7) {} // ERROR
  if (((false?2:3)?4:5)?6:7) {} // ERROR
  if (((('a'?2:3)?4:5)?6:7)?8:9) {} // ERROR
  if ((((true?2:3)?4:5)?6:7)?8:9) {} // ERROR
  if ((((false?2:3)?4:5)?6:7)?8:9) {} // ERROR
}

{
  declare var cond: boolean;
  let v;
  const x1 = (cond ? [] : null) ?? 'A'; // OK, [] is truthy, null is falsy
  const x2 = (cond ? [] as Array<number> : null) ?? 'A'; // OK, [] is truthy, null is falsy, testing recursing into the arg of `AsExpression`
  const x3 = (cond ? ([]: Array<number>): null) ?? 'A'; // OK
  const x4 = (cond ? [] as const : null) ?? 'A'; // OK
  const x5 = (cond ? v=[] : null) ?? 'A'; // OK
  const x6 = (cond ? (3,4,5) : null) ?? 'A'; // OK
}
