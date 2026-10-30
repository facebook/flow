class AssignmentOperators {
  first: string;
  second: string;
  third: string; // error
  fourth: number; // error
  fifth: number; // error
  constructor(value: string) {
    this.first ||= value;
    this.second ??= value;
    this.third &&= value;
    this.fourth += 1;
    this.fifth++;
  }
}
