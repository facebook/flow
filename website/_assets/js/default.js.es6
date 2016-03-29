//= require analytics
//= require linkify

import {linkifyHeader} from "linkify"
import * as docsearch from "docsearch"

[].slice.apply(
  document.querySelectorAll(
    'article h2, article h3, article h4'
  )
).forEach(linkifyHeader);

docsearch({
  apiKey: 'cf314b2976b27157c82b6f27553497f5',
  indexName: 'flowtype',
  inputSelector: '#algolia-doc-search'
});
