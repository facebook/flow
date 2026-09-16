// @flow

const flowTuple: [string, string] = ['a', 'b'];
const flowTupleArray: Array<[string, string]> = [flowTuple];

acceptsTSStrings(flowTuple);
acceptsTSStringArrays(flowTupleArray);

function acceptsFlowStrings(values: Array<string>): void {}
function acceptsFlowStringArrays(values: Array<Array<string>>): void {}

acceptsFlowStrings(flowTuple); // ERROR
acceptsFlowStringArrays(flowTupleArray); // ERROR

acceptsFlowStrings(tsTuple);
acceptsFlowStringArrays(tsTupleArray);

const prefixed: string = 'id-' + returnsTSTemplateString();
const suffixed: string = returnsTSTemplateString() + '-id';
