new DoublyLinkedList().prev() as DoublyLinkedList;
new DoublyLinkedList().next() as DoublyLinkedList

var MiniImmutable = require("mini-immutable");
class C {
  map: MiniImmutable.OrderedMap<number,string>;
  update() {
    this.map = this.map.set(0,"");
  }
}
