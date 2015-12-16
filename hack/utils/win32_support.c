/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <limits.h>
#include <math.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/misc.h>
#include <stdint.h>

#ifdef _WIN32
#include <Windows.h>
#endif

// The following 'typedef' won't be required anymore
// when dropping support for OCaml < 4.03
#ifdef __MINGW64__
typedef  __int64 int64_t;
#endif

/* Can be removed once the PR #208 will be merged in OCaml */
/* (https://github.com/ocaml/ocaml/pull/268). */
/* This code handle hexadecimal notation for floating-point numbers */
static int hh_float_of_hex(const char * s, double * res)
{
  int64_t m = 0;                /* the mantissa - top 60 bits at most */
  int n_bits = 0;               /* total number of bits read */
  int m_bits = 0;               /* number of bits in mantissa */
  int x_bits = 0;               /* number of bits after mantissa */
  int dec_point = -1;           /* bit count corresponding to decimal point */
                                /* -1 if no decimal point seen */
  int exp = 0;                  /* exponent */
  char * p;                     /* for converting the exponent */

  while (*s != 0) {
    char c = *s++;
    switch (c) {
    case '_':
      break;
    case '.':
      if (dec_point >= 0) return -1; /* multiple decimal points */
      dec_point = n_bits;
      break;
    case 'p': case 'P': {
      long e;
      if (*s == 0) return -1;   /* nothing after exponent mark */
      e = strtol(s, &p, 10);
      if (*p != 0) return -1;   /* ill-formed exponent */
      if (e < INT_MIN || e > INT_MAX) return -1; /* unreasonable exponent */
      exp = e;
      s = p;                    /* stop at next loop iteration */
      break;
    }
    default: {                  /* Nonzero digit */
      int d;
      if (c >= '0' && c <= '9') d = c - '0';
      else if (c >= 'A' && c <= 'F') d = c - 'A' + 10;
      else if (c >= 'a' && c <= 'f') d = c - 'a' + 10;
      else return -1;           /* bad digit */
      n_bits += 4;
      if (d == 0 && m == 0) break; /* leading zeros are skipped */
      if (m_bits < 60) {
        /* There is still room in m.  Add this digit to the mantissa. */
        m = (m << 4) + d;
        m_bits += 4;
      } else {
        /* We've already collected 60 significant bits in m.
           Now all we care about is whether there is a nonzero bit
           after. In this case, round m to odd so that the later
           rounding of m to FP produces the correct result. */
        if (d != 0) m |= 1;        /* round to odd */
        x_bits += 4;
      }
      break;
    }
    }
  }
  /* Convert mantissa to FP.  We use a signed conversion because we can
     (m has 60 bits at most) and because it is faster
     on several architectures. */
  double f = (double) (int64_t) m;
  /* Adjust exponent to take decimal point and extra digits into account */
  if (dec_point >= 0) exp = exp + (dec_point - n_bits);
  exp = exp + x_bits;
  /* Apply exponent if needed */
  if (exp != 0) f = ldexp(f, exp);
  /* Done! */
  *res = f;
  return 0;
}

value hh_float_of_string(value vs)
{
  char parse_buffer[64];
  char * buf, * src, * dst, * end;
  mlsize_t len;
  int sign;
  double d;

  /* Check for hexadecimal FP constant */
  src = String_val(vs);
  sign = 1;
  if (*src == '-') { sign = -1; src++; }
  else if (*src == '+') { src++; };
  if (src[0] == '0' && (src[1] == 'x' || src[1] == 'X')) {
    if (hh_float_of_hex(src + 2, &d) == -1)
      caml_failwith("float_of_string");
    return caml_copy_double(sign < 0 ? -d : d);
  }
  /* Remove '_' characters before calling strtod () */
  len = caml_string_length(vs);
  buf = len < sizeof(parse_buffer) ? parse_buffer : caml_stat_alloc(len + 1);
  src = String_val(vs);
  dst = buf;
  while (len--) {
    char c = *src++;
    if (c != '_') *dst++ = c;
  }
  *dst = 0;
  if (dst == buf) goto error;
  /* Convert using strtod */
  d = strtod((const char *) buf, &end);
  if (end != dst) goto error;
  if (buf != parse_buffer) caml_stat_free(buf);
  return caml_copy_double(d);
 error:
  if (buf != parse_buffer) caml_stat_free(buf);
  caml_failwith("float_of_string");
  return Val_unit; /* not reached */
}
