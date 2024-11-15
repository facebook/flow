declare export component CompWithoutRefProp(foo: string);
declare export component CompWithOptionalRefProp(foo: string, ref?: React.RefSetter<HTMLElement>);
declare export component CompWithRequiredRefProp(foo: string, ref: React.RefSetter<HTMLElement>);
