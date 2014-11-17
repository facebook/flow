[].slice.apply(
  document.querySelectorAll(
    'article h2, article h3, article h4'
  )
).forEach(function(header) {
  var slug = header.innerText
    .toLowerCase()
    .replace(/[^a-z0-9]/g, '-')
    .replace(/-+/g, '-')
    .replace(/^-|-$/g, '');

  var hashref = document.createElement('a');
  hashref.id = slug;
  hashref.className = 'hashref';
  header.appendChild(hashref);

  var hash = document.createElement('a');
  hash.className = 'hash';
  hash.href = '#' + slug;
  hash.innerText = '#';
  header.appendChild(hash);
});
