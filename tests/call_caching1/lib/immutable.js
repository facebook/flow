declare module "immutable" {
  declare class Map<K,V> {
    static <K,V>(iter: Iterator<[K,V]>): Map<K,V>;
    static <K extends string,V>(object: interface {+[k:K]:V}): Map<K,V>;

    set(key: K, value: V): this;
  }
}
