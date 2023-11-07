//@flow

declare var invariant: any;

let aa: string | number = 42;
let havoc = function () {
  aa = 'a';
};
invariant(typeof aa === 'number');

{
  let aa: string | number = 42;
  invariant(typeof aa === 'number');
  havoc();
  aa as number; // fine
}

aa as number; // fail
