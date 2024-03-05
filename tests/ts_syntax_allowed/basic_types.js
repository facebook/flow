null as undefined; // error
undefined as undefined; // ok

3 as unknown; // ok
'' as unknown; // ok
'' as unknown as never; // error
(() => {throw ''})() as never as 3; // ok
