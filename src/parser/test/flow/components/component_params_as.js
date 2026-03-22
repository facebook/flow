component Comp1(param as p_local) {}

component Comp2(param as p_local: TParam) {}

component Comp3('param' as p_local) {}

component Comp4(param as {p_local}) {}

component Comp5(param as [p_local]) {}

component Comp6('string-key' as alias?: string) {}

component Comp7('aria-label' as ariaLabel?: string) {}
