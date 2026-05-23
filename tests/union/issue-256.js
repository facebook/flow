declare class Myclass {
    myfun(myarray: Array<any | string>): any;
}
declare const myclass: Myclass;

myclass.myfun(["1", "2", "3", "4", "5", "6", function (ar) {}])
myclass.myfun(["1", "2", "3", "4", "5", "6", "7", function (ar) {}])
