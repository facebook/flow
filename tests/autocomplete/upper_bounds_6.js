//@flow

declare var f : {| foo: number, bar: string, 'foo.bar': boolean, '@@iterator': any |} => void;

f({      //
//  ^
