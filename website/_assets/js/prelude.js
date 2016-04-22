//= require requirejs

require.config({
  baseUrl: '/assets',
  paths: {
    'docsearch': 'https://cdn.jsdelivr.net/docsearch.js/1/docsearch.min',
    'typekit': 'https://use.typekit.net/vqa1hcx'
  },
  waitSeconds: 30
})

require(['typekit'], function() {
  try{
    Typekit.load({ async: true });
  } catch (e) {}
});
