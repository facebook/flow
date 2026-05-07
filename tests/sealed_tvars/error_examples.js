//@flow

declare const values: {
    greeting: string,
};

if (values.greeting === values) {} // constant-condition error
