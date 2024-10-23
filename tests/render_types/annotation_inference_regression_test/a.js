declare export component C()
type ComponentThatRendersC<Props: {...}> = component(...Props) renders C;
export const C2: ComponentThatRendersC<{}> = C;
