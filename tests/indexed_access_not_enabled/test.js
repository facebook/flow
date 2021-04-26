type Obj = {
  foo: boolean,
};

type ObjFoo = Obj['foo']; // Error
('invalid': ObjFoo); // OK - is `any`

type MaybeObj = ?Obj;
type MaybeObjFoo = Obj?.['foo']; // Error
('invalid': MaybeObjFoo); // OK - is `any`
