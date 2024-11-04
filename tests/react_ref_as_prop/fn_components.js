declare export function FnWithoutRefProp(props: {foo: string}): React.Node;
declare export function FnWithOptionalRefProp(props: {foo: string, ref?: React.RefSetter<HTMLElement>}): React.Node;
declare export function FnWithRequiredRefProp(props: {foo: string, ref: React.RefSetter<HTMLElement>}): React.Node;
