/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * This file exports a customized fast comparison function
 * for most common cases, it does not cover edge cases like OCaml's built-in
 * ones. And its string comparision is based on length first, and content later.
 *
 * The restrictions are documented in the
 * exported functions.
 *
 * DON'T USE IT until it is confirmed that such
 * comparison is indeed a hot path and `ppx_deriving` is
 * too complicated for your type.
 *
 */

#include <caml/mlvalues.h>
#include <string.h>
#define CAML_NAME_SPACE
#define INIT_SIZE 8
#define LESS Val_int(-1)
#define EQUAL Val_int(0)
#define GREATER Val_int(1)

typedef struct {
  value* v1;
  value* v2;
  mlsize_t count;
} item;

typedef struct {
  item init_stack[INIT_SIZE];
  item* stack;
  item* limit;
} stack;

// Resize the stack and modify the
// current point in place
static void resize_stack(stack* stk, item** sp) {
  asize_t newsize = 64;
  asize_t sp_offset = *sp - stk->stack;
  item* newstack;

  if (stk->stack == stk->init_stack) {
    newstack = malloc(sizeof(item) * newsize);
    memcpy(newstack, stk->init_stack, sizeof(item) * INIT_SIZE);
  } else {
    newsize = 2 * (stk->limit - stk->stack);
    newstack = realloc(stk->stack, sizeof(item) * newsize);
  }
  stk->stack = newstack;
  stk->limit = newstack + newsize;
  *sp = newstack + sp_offset;
}

/**
 * compare [v1] [v2] when they are
 * immediate numbers
 */
#define MAYBE_LONG_COMPARE(v1, v2) \
  do {                             \
    if (Is_long(v1)) {             \
      if (Is_long(v2)) {           \
        if (v1 < v2) {             \
          return LESS;             \
        }                          \
        return GREATER;            \
      }                            \
      return LESS;                 \
    }                              \
    if (Is_long(v2)) {             \
      return GREATER;              \
    }                              \
  } while (0)

/**
 * comapre [v1] [v2] assuming they are
 * immediate numbers
 */
#define SIMPLE_COMPARE(v1, v2) \
  do {                         \
    if (v1 != v2) {            \
      if (v1 < v2) {           \
        return LESS;           \
      }                        \
      return GREATER;          \
    }                          \
  } while (0)

/**
 * comparing [v1] [v2] assuming they are
 * ocaml doubles
 */
#define DOUBLE_VAL_COMPARE(v1, v2) \
  do {                             \
    double d1 = Double_val(v1);    \
    double d2 = Double_val(v2);    \
    if (d1 < d2)                   \
      return LESS;                 \
    if (d1 > d2)                   \
      return GREATER;              \
  } while (0)

/**
 * comparing [v1] [v2] assuming they are
 * ocaml strings
 *
 */
#define STRING_VAL_COMPARE(v1, v2)                          \
  do {                                                      \
    int len1 = caml_string_length(v1);                      \
    int len2 = caml_string_length(v2);                      \
    SIMPLE_COMPARE(len1, len2);                             \
    int res = memcmp(String_val(v1), String_val(v2), len1); \
    if (res < 0)                                            \
      return LESS;                                          \
    if (res > 0)                                            \
      return GREATER;                                       \
  } while (0)

/**
 * PUSH_STACK(sp,pt1,pt2,sz1)
 * assuming that pt1[0] is visited while
 * pt1[1] will be put on stack
 */
#define PUSH_STACK(sp, pt1, pt2, sz1) \
  do {                                \
    sp->v1 = &Field(pt1, 1);          \
    sp->v2 = &Field(pt2, 1);          \
    sp->count = sz1;                  \
  } while (0)

/**
 * POP_STACK(sp,stk,val1,val2)
 * POP the stack the poped value is put inside [val1]
 * and [val2]
 */
#define POP_STACK(sp, stk, val1, val2) \
  do {                                 \
    if (sp == stk->stack)              \
      return EQUAL;                    \
    val1 = *((sp->v1)++);              \
    val2 = *((sp->v2)++);              \
    if (--(sp->count) == 0)            \
      sp--;                            \
  } while (0)

static value compare_with_stack(value v1, value v2, stack* stk, item* sp) {
  while (1) {
    if (v1 != v2) {
      MAYBE_LONG_COMPARE(v1, v2);
      tag_t t1 = Tag_val(v1);
      tag_t t2 = Tag_val(v2);
      SIMPLE_COMPARE(t1, t2);
      switch (t1) {
        case String_tag: {
          STRING_VAL_COMPARE(v1, v2);
          break;
        }
        case Double_tag: {
          DOUBLE_VAL_COMPARE(v1, v2);
          break;
        }
        default: {
          mlsize_t sz1 = Wosize_val(v1);
          mlsize_t sz2 = Wosize_val(v2);
          // when tag is the same while
          // size is different -- this seems only possible for array
          // v1 != v2 && sz1 = sz2 = 0 && Tag(v1) = Tag(v2) holds, since Atom
          // are preallocated
          SIMPLE_COMPARE(sz1, sz2);
          if (--sz1 > 0) {
            sp++;
            if (sp >= stk->limit)
              resize_stack(stk, &sp);
            PUSH_STACK(sp, v1, v2, sz1);
          }
          v1 = Field(v1, 0);
          v2 = Field(v2, 0);
          continue;
        }
      }
    }

    POP_STACK(sp, stk, v1, v2);
  }
}

/**
 * compare two values [v1] and [v2] which foucs on performance
 * restrictions:
 * - The entry point of [v1] and [v2] should not be an
 *   array, string or double
 * - the child block should not contain
 *   - ocaml style objects
 *   - custom block Int32.t, Int64.t and Lazy.t
 * Due to these restrictions, this function is only used
 * in [Type.ml] [TypeTerm.use_t] and [TypeTerm.t]
 */
CAMLprim value caml_fast_generic_compare(value v1, value v2) {
  stack stk;

  if (v1 != v2) {
    MAYBE_LONG_COMPARE(v1, v2);

    tag_t t1 = Tag_val(v1);
    tag_t t2 = Tag_val(v2);
    SIMPLE_COMPARE(t1, t2);
    mlsize_t sz1 = Wosize_val(v1);

    item* sp = stk.stack = stk.init_stack;
    stk.limit = stk.stack + INIT_SIZE;
    if (--sz1 > 0) {
      sp++;
      PUSH_STACK(sp, v1, v2, sz1);
    }
    v1 = Field(v1, 0);
    v2 = Field(v2, 0);
    value res = compare_with_stack(v1, v2, &stk, sp);
    if (stk.stack != stk.init_stack) {
      free(stk.stack);
    }
    return res;
  }
  return EQUAL;
}
