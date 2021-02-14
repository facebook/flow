//@flow
// Example provided by @TrySound on github
type Query = { [string]: number };
const query: Query = {};
const {page, language, ...params} = query;
const query2: Query = params; // Should not error
