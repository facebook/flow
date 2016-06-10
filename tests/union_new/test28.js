// @noflow

const {
  TABLE_WIDTH,
  BASE_COLUMNS,
} = require('./test28-helper');

const columns = [...BASE_COLUMNS];
for (let i = 0; i < 1000; ++i) {
  const dataKey = (i % 4) + 1;
  columns.push({
    key: `${dataKey}`,
    width: Math.round(TABLE_WIDTH / 16),
    extra: {
      dataKey,
      label: `${i}`,
    },
  });
}
