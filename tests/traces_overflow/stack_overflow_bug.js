// @flow

const A = 'yyyyy';
const B = 'zzz_xxxxx';
const C = 'zzz_xxx_xxx';
const D = 'zzz_yyy_yyy';
const E = 'sub_xxxxxxx';
const F = 'yyyyyy_xxxx';

const DATA = [
  [A, B, C, D, E, F, '1'],
  [A, B, C, D, E, F, '2'],
  [A, B, C, D, E, F, '3'],
  [A, B, C, D, E, F, '4'],
  [A, B, C, D, E, F, '5'],
  [
    '"""123456789123456"""',
    'yyyyyy',
    '20000000001234567890ABCDEFABCDEF',
    '21111111111234567890ABCDEFABCDEF',
    'xxx yyyy 1',
    'ccccccccccc',
  ],
  [
    '1"""123456789123456"""',
    '1yyyyyy',
    '120000000001234567890ABCDEFABCDEF',
    '121111111111234567890ABCDEFABCDEF',
    '1xxx yyyy 1',
    'ddddddddddd',
  ],
  [
    '2"""123456789123456"""',
    '2yyyyyy',
    '220000000001234567890ABCDEFABCDEF',
    '221111111111234567890ABCDEFABCDEF',
    '2xxx yyyy 1',
    'ggggggggg',
  ],
  [
    '"""123456789123456"""',
    'INyyyyyy',
    '20000000001234567890ABCDEFABCDEF',
    '21111111111234567890ABCDEFABCDEF',
    'fffffffff',
  ],
];

const LINK = `data:text/csv;charset=utf-8,${DATA.map(
  row => row.join(','),
).join(`\n`)}`;

function foo(row: Array<string>) {
  const data = DATA[0].reduce((accumulator, colName, idx) => {
    return {
      ...accumulator,
      [colName]: row[idx]
    };
  }, {});
}
