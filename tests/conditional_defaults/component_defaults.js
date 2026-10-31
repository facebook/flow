let assigned: number | string = 'before';

component PatternDefaultAssignment(props as {flag = ((assigned = 1), true)}: {flag?: boolean}) {
  assigned as number; // error
  return null;
}

// Not reported yet: the defaults of a component's own parameters are still treated as always evaluated.
component ParameterDefaultAssignment(flag: boolean = ((assigned = 1), true)) {
  assigned as number;
  return null;
}
