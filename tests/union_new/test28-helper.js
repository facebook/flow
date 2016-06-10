// @noflow

const TABLE_WIDTH = 500;

const BASE_COLUMNS = [
  {
    key: '0',
    width: Math.round(TABLE_WIDTH / 6),
    isFixed: true,
    extra: {
      dataKey: 0,
      label: 'ID',
    },
  },
  {
    key: '1',
    width: Math.round(TABLE_WIDTH / 3),
    isFixed: true,
    extra: {
      dataKey: 1,
      label: 'A',
    },
  },
  {
    key: '2',
    width: Math.round(TABLE_WIDTH / 4),
    extra: {
      dataKey: 2,
      label: 'B',
    },
  },
  {
    key: '3',
    width: Math.round(TABLE_WIDTH / 2),
    extra: {
      dataKey: 3,
      label: 'C',
    },
  },
  {
    key: '4',
    width: 200,
    extra: {
      dataKey: 4,
      label: 'D',
    },
  },
  {
    key: '5',
    width: 200,
    extra: {
      dataKey: 5,
      label: 'E',
    },
  },
  {
    key: '6',
    width: 200,
    extra: {
      dataKey: 6,
      label: 'F',
    },
  },
];

module.exports = {
  TABLE_WIDTH,
  BASE_COLUMNS,
};
