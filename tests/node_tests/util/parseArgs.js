const {parseArgs} = require('util');

const {positionals, values} = parseArgs({
  options: {
    boolean: {
      type: 'boolean',
    },
    booleanSingular: {
      type: 'boolean',
      multiple: false,
    },
    booleanMultiple: {
      type: 'boolean',
      multiple: true,
    },
    string: {
      type: 'string',
    },
    stringSingular: {
      type: 'string',
      multiple: false,
    },
    stringMultiple: {
      type: 'string',
      multiple: true,
    },
  }
});

(positionals: Array<string>);

(values.boolean: boolean);
(values.booleanSingular: boolean);
(values.booleanMultiple: Array<boolean>);

(values.string: string);
(values.stringSingular: string);
(values.stringMultiple: Array<string>);

values.invalid; // error, prop-missing

parseArgs({}).tokens; // error, prop-missing

parseArgs({tokens: false}).tokens; // error, prop-missing

parseArgs({tokens: true}).tokens;
