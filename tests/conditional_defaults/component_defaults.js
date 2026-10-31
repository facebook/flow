let assigned: number | string = 'before';

component PatternDefaultAssignment(props as {flag = ((assigned = 1), true)}: {flag?: boolean}) {
  assigned as number; // error
  return null;
}

component ParameterDefaultAssignment(flag: boolean = ((assigned = 1), true)) {
  assigned as number; // error
  return null;
}
