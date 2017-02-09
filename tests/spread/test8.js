/* @flow */
type Test = {
    id: number,
}

type TestSubclass = Test & { info: string, } ;

type TestSubclass2 = {
    id: number,
    info: string,
}

function one(props: TestSubclass) {
    return props;
}

class FailingClass {
    props: TestSubclass;
    two() {
        return one({...this.props});
    }
}

class WorkingClass1 {
    props: TestSubclass;
    two() {
        return one(this.props);
    }
}

class WorkingClass2 {
    props: TestSubclass;
    two() {
        let temp_props: TestSubclass = this.props;
        return one({...temp_props});
    }
}

class WorkingClass3 {
    props: TestSubclass;
    props = {id: 1, info: 'uueua'}
    two() {
        return one({...this.props});
    }
}

class WorkingClass4 {
    two(props: TestSubclass) {
        return one({...props});
    }
}

class WorkingClass5 {
    two(props: {a: TestSubclass}) {
        return one({...props.a});
    }
}

class WorkingClass6 {
    props: TestSubclass2;
    two() {
        return one({...this.props});
    }
}

