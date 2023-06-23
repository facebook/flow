The Flow_sedlexing_ppx module is forked from the Sedlexing_ppx module, with the
following changes:

- Delete Unicode generator
- Inlined Unicode module into ppx_sedlex, and only keep the character set we need
- Remove charsets not needed by Flow in sedlex_cset
- Dune build rule tweaks
