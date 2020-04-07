class Test {
    /* 1.1 L id */ prop /* 1.2 T id */;

    /* 2.1 L prop */ declare /* 2.2 L prop */ static /* 2.3 L variance */ + /* 2.4 L id */ prop /* 2.5 T id */;

    /* 3.1 L private prop */ declare /* 3.2 L private prop */ static /* 3.3 L variance */ + /* 3.4 L private */ #private1 /* 3.5 T id */;

    /* 4.1 L id */ prop /* 4.2 T id */ = /* 4.3 L num */ 1 /* 4.4 T num */;

    /* 5.1 L private */ #private2 /* 5.2 T id */ = /* 5.3 L num */ 1 /* 5.4 T num */;

    /* 6.1 L computed */ [prop] /* 6.2 T computed */ = /* 6.3 L num */ 1 /* 6.4 T num */;
}
