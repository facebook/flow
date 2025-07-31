//@flow

declare var values: {
    greeting: string,
};

if (values.greeting === values) {} // error, to be flagged invalid comparison
