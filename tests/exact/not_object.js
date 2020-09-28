//@flow

let x : {||} = 3;
let y : {||} = "";
let z : {||} = true;

let c : {||} = new class {};
let f : {||} = () => {};

declare var u : {||} | {};
(u : {||});

declare var v : number | {};
(v : {||});


class A {}

(0: $Exact<number>);
("": $Exact<string>);
(true: $Exact<boolean>);
(new A(): $Exact<A>);
(()=>{} : $Exact<()=>{}>);


let foo = () => {};
(()=>{} : $Exact<typeof foo>);
let bar = new A;
(new A(): $Exact<typeof bar>);
