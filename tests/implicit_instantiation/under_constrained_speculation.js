// @flow

(0: string); // error: intentially left here to ensure that implicit instantiation
             // below does not reset it

declare export class C<K, +V> {
  static <K, V>(_: void): C<K, V>;
  static <K, V>(iter: Array<[K, V]>): C<K, V>;
}

declare function foo<K, +V>(_: void): C<K, V>;
declare function foo<K, V>(iter: Array<[K, V]>): C<K, V>;

declare var any_: any;

C(any_); // okay, we infer C<any, any>
C(undefined); // error branches are under-constrained for the first overload,
              // and incompatible void ~> array for the second


foo(any_);
foo(undefined); // error branches are under-constrained for the first overload,
                // and incompatible void ~> array for the second
