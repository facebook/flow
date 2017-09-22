(* get_rtdsc_time is a wrapper around the RSTDC instruction,
 * this counts the number of instructions since the counter was
 * last reset for the processor that you're on.
 *
 * The number is logically an unsigned 64 bit number. A machine-precision
 * integer might not be big enough to represent this number perfectly, so
 * a float is returned instead. This follows the convention used in, e.g.
 * The Unix module that ships with OCaml:
 *
 *    val time : unit -> float
 *    Return the current time since 00:00:00 GMT, Jan. 1, 1970, in seconds.
 *
 * This API deliberately does not expose a way to reset the counter.
 *
 * There are a bunch of caveats about what happens if your
 * process is interrupted or if you get scheduled onto a different
 * processor.
 * *)
external get_rtdsc_time : unit -> float = "ocaml_rtdsc";;
