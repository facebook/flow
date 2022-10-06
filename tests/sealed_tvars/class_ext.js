//@flow

class C extends ((42: any): D).f(x => 42) {}

class D {
    f(x: mixed): any {
        return C;
    }
}
