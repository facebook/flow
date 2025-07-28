Base64 for OCaml
================

Base64 is a group of similar binary-to-text encoding schemes that represent
binary data in an ASCII string format by translating it into a radix-64
representation.  It is specified in [RFC 4648][rfc4648].

See also [documentation][docs].

[rfc4648]: https://tools.ietf.org/html/rfc4648
[docs]: http://mirage.github.io/ocaml-base64/base64/

## Example

Simple encoding and decoding.

```shell
utop # #require "base64";;
utop # let enc = Base64.encode_exn "OCaml rocks!";;
val enc : string = "T0NhbWwgcm9ja3Mh"
utop # let plain = Base64.decode_exn enc;;
val plain : string = "OCaml rocks!"
```

## License

[ISC](https://www.isc.org/downloads/software-support-policy/isc-license/)
