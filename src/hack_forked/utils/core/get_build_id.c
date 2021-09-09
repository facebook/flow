/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * Export the constants provided by Facebook's build system to ocaml-land, since
 * their FFI only allows you to call functions, not reference variables. Doing
 * it this way makes sense for Facebook internally since our build system has
 * machinery for providing these two constants automatically (and no machinery
 * for doing codegen in a consistent way to build an ocaml file with them) but
 * is very roundabout for external users who have to have CMake codegen these
 * constants anyways. Sorry about that.
 */

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <string.h>
extern const char *const BuildInfo_kRevision;

value hh_get_build_revision(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(caml_copy_string(BuildInfo_kRevision));
}

#define INIT_SIZE 8
#define LESS Val_int(-1)
#define EQUAL Val_int(0)
#define GREATER Val_int(1)

typedef struct
{
  value *v1;
  value *v2;
  mlsize_t count;
} item;

typedef struct
{
  item init_stack[INIT_SIZE];
  item *stack;
  item *limit;
} stack;

// Resize the stack and modify the
// current point in place
static void resize_stack(stack *stk,
                         item **sp)
{
  asize_t newsize = 64;
  asize_t sp_offset = *sp - stk->stack;
  item *newstack;

  if (stk->stack == stk->init_stack)
  {
    newstack = malloc(sizeof(item) * newsize);
    memcpy(newstack, stk->init_stack,
           sizeof(item) * INIT_SIZE);
  }
  else
  {
    newsize = 2 * (stk->limit - stk->stack);
    newstack = realloc(stk->stack,
                       sizeof(item) * newsize);
  }
  stk->stack = newstack;
  stk->limit = newstack + newsize;
  *sp = newstack + sp_offset;
}

// compare two immediate numbers
// they can not be equal
#define LONG_COMPARE(v1, v2) \
  do                         \
  {                          \
    if (Is_long(v1))         \
    {                        \
      if (Is_long(v2))       \
      {                      \
        if (v1 < v2)         \
        {                    \
          return LESS;       \
        }                    \
        return GREATER;      \
      }                      \
      return LESS;           \
    }                        \
    if (Is_long(v2))         \
    {                        \
      return GREATER;        \
    }                        \
  } while (0)

// compare two imemediate numbers
// They could be equal
#define SIMPLE_COMPARE(v1, v2) \
  do                           \
  {                            \
    if (v1 != v2)              \
    {                          \
      if (v1 < v2)             \
      {                        \
        return LESS;           \
      }                        \
      return GREATER;          \
    }                          \
  } while (0)

// compare two floating numbers
// need take care of NaN cases
#define FLOAT_COMPARE(d1, d2) \
  do                          \
  {                           \
    if (d1 < d2)              \
      return LESS;            \
    if (d1 > d2)              \
      return GREATER;         \
    if (d1 != d2)             \
    {                         \
      if (d1 == d1)           \
        return GREATER;       \
      if (d2 == d2)           \
        return LESS;          \
    }                         \
  } while (0)

#define PUSH_STACK(sp, v1, v2, sz1) \
  do                                \
  {                                 \
    sp->v1 = &Field(v1, 1);         \
    sp->v2 = &Field(v2, 1);         \
    sp->count = sz1;                \
  } while (0)

#define POP_STACK(sp, stk, v1, v2) \
  do                               \
  {                                \
    if (sp == stk->stack)          \
      return EQUAL;                \
    v1 = *((sp->v1)++);            \
    v2 = *((sp->v2)++);            \
    if (--(sp->count) == 0)        \
      sp--;                        \
  } while (0)

static value compare_with_stack(
    value v1, value v2, stack *stk, item *sp)
{

  while (1)
  {
    if (v1 != v2)
    {

      LONG_COMPARE(v1, v2);
      tag_t t1 = Tag_val(v1);
      tag_t t2 = Tag_val(v2);
      SIMPLE_COMPARE(t1, t2);
      switch (t1)
      {
      case String_tag:
      {
        // We only need a total order here
        // Here the string comparison is different from the one used
        // with lexical semantics
        int len1 = caml_string_length(v1);
        int len2 = caml_string_length(v2);
        int res = memcmp(String_val(v1), String_val(v2), (len1 < len2) ? len1 : len2);
        if (res < 0)
          return LESS;
        if (res > 0)
          return GREATER;
        if (len1 < len2) {
          return LESS;
        }
        else if (len1 > len2) {
          return GREATER;
        }
        break; // could be equal
      }
      case Double_tag:
      {
        double d1 = Double_val(v1);
        double d2 = Double_val(v2);
        FLOAT_COMPARE(d1, d2);
        break; // could be equal
      }
      default:
      {
        mlsize_t sz1 = Wosize_val(v1);
        mlsize_t sz2 = Wosize_val(v2);
        // when tag is the same while
        // size is different -- this seems only possible for array
        // v1 != v2 && sz1 = sz2 = 0 && Tag(v1) = Tag(v2) holds, since Atom are preallocated
        SIMPLE_COMPARE(sz1, sz2);
        if (--sz1 > 0)
        {
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

CAMLprim value caml_fast_generic_compare(value v1, value v2)
{
  stack stk;

  if (v1 != v2)
  {
    LONG_COMPARE(v1, v2);

    tag_t t1 = Tag_val(v1);
    tag_t t2 = Tag_val(v2);
    SIMPLE_COMPARE(t1, t2);
    mlsize_t sz1 = Wosize_val(v1);

    item *sp = stk.stack = stk.init_stack;
    stk.limit = stk.stack + INIT_SIZE;
    if (--sz1 > 0)
    {
      sp++;
      PUSH_STACK(sp, v1, v2, sz1);
    }
    v1 = Field(v1, 0);
    v2 = Field(v2, 0);
    value res = compare_with_stack(v1, v2, &stk, sp);
    if (stk.stack != stk.init_stack)
    {
      free(stk.stack);
    }
    return res;
  }
  return EQUAL;
}
