const arr = [1,2,3];  // badly-positioned error

// fun.call()
(function (this: any, ...args: any) { return this.bar; }).call(...arr); // error

// fun.apply()
(function (this: any) { return this.bar; }).apply(...arr); // error
(function (this: any) { return this.bar; }).apply(({}: any), ...arr); // error
(function (this: any) { return this.bar; }).apply(...arr, ...arr); // error
