async function f() {}
async function ft<T>(a: T): void {}

class C {
  async m() {}
  async mt<T>(a: T): void {}
  static async m(a): void {}
  static async mt<T>(a: T): void {}
}

var e = async function () {};
var et = async function<T> (a: T): void {};

var n = new async function() {};

var o = { async m() {} };
var ot = { async m<T>(a: T) {} };
var oz = { async async() {} };

var x = { async : 5 };
console.log(x.async);

var async = 3;
var y = { async };
