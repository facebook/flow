function foo<T>(x: T) {}

function trailingOnly() {
    foo() /* 1.1 Trailing */;
}

function allComments() {
    /* 2.1 L id */ foo /* 2.2 T id */ </* 2.3 L type */ number /* 2.4 T type */>(/* 2.5 L num */ 1 /* 2.6 T num */) /* 2.7 T call */;
}

function implicitCallTypeArgComments() {
    foo</* 3.1 L implicit */ _ /* 3.2 T implicit */>(1);
}
