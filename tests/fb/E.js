var E = React.createClass({
    mixins: [ReactGraphQL.Mixin],
    statics: {
        foo() { },
        queries: { hmm: 0 }
    },
    bar() { }
});
E.getQuery();
module.exports = E;
