declare class LinkedList {
  next(): this;
}
declare class DoublyLinkedList extends LinkedList {
  prev(): this;
}

declare module "mini-immutable" {
  declare class Map<K,V> {
    set(key: K, value: V): this; // more precise than Map<K,V> (see below)
  }
  declare class OrderedMap<K,V> extends Map<K,V> {
    // inherits set method returning OrderedMap<K,V> instead of Map<K,V>
  }
}
