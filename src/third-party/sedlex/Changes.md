The Flow_sedlexing module is forked from the Sedlexing module, with the
following changes:

- Removed encodings we don't need, e.g, Utf16, Latin1
- Removed bookkeeping fields which do not matter when everything is done in memory:
  - filename
  - finished
  - refilled (refill not needed either)
- Changed Uchar.t array to int array
- Added lexbuf_clone for internal usage
- Added 
  - Utf8.lexeme_to_buffer
  - Utf8.lexeme_to_buffer2
- Rewrote utf8 encoding/decoding for performance
