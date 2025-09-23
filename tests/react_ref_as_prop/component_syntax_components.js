declare export component CompWithoutRefProp(foo: string);
declare export component CompWithOptionalRefProp(foo: string, ref?: React.RefSetter<HTMLElement>);
declare export component CompWithRequiredRefProp(foo: string, ref: React.RefSetter<HTMLElement>);

component Has_ref_prop_but_do_not_require_react_to_be_in_scope(ref?: any) { // ok: since we have full ref-as-prop support
    return;
}
