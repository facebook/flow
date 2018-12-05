//@flow

declare var x: React$AbstractComponent<{foo: number}, {foo: number}, void>;

(x.defaultProps: {foo: number});
x.defaultProps = {foo: 3};

(x.defaultProps: {foo: number, bar: number}); // Error, bar not in defaultProps.
x.defaultProps = {foo: 3, bar: 3}; // Ok, default props still hold, but won't be reflected in
                                   // config calculation.

x.defaultProps = {bar: 3}; // error, missing foo.
