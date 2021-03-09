type Obj = {
  foo: boolean,
};

type ObjFoo = Obj['foo']; // Error
('invalid': ObjFoo); // OK - is `any`
