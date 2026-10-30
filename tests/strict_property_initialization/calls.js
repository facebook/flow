declare function log(message: string): void;

class AssignThenCall {
  value: string;
  constructor() {
    this.value = 'hi';
    log('assigned');
    this.value;
  }
}

class AssignmentInMethodNotFollowed {
  value: string; // error
  constructor() {
    this.init();
  }
  init(): void {
    this.value = 'hi';
  }
}
