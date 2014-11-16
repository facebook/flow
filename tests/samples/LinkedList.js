class Node {
  constructor() {
      this.x = this.x;
  }
}

class Token extends Node {}

class LinkedList {
  constructor(node) {
    this.head = node;
  }
  bar() {
    this.head.x = this.head;
    this.head = this.head.x;
  }

}

new LinkedList(new Token()).bar();
