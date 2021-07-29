

It did several things:

- Removed things we don't care, e.g, Utf16, Latin1

- Removed bookkeeping fields which does not matter when everything is done in memory:
  - filename
  - finished
  - refilled (refill not needed either)

- Changed Uchar.t array to int array

- Added lexbuf_clone for internal usage
- Added 
  - Utf8.lexeme_to_buffer
  - Utf8.lexeme_to_buffer2

- Rewrite utf8 encoding/decoding in a single while loop
  instead of jumping lots of hoops.

