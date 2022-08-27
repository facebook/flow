async function f() {}
async function ft<T>(a: T) {}

class C {
  async m(): Promise<void> {}
  async mt<T>(a: T): Promise<void> {}
  static async m(a: mixed): Promise<void> {}
  static async mt<T>(a: T): Promise<void> {}
}

var e = async function () {};
var et = async function<T> (a: T) {};

var o = { async m(): Promise<void> {} };
var ot = { async m<T>(a: T): Promise<void> {} };
var oz = { async async(): Promise<void> {} };

var x = { async : 5 };
console.log(x.async);

var async = 3;
var y = { async };
