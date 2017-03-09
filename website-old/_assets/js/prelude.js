//= require requirejs/require
//= link_tree ../gen

require.config({
  baseUrl: '/assets',
  paths: {
    'docsearch': 'https://cdn.jsdelivr.net/docsearch.js/1/docsearch.min',
  },
  waitSeconds: 30
})
