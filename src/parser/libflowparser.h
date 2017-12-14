/**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

namespace flowparser {

static value list_head(value props) {
  return Field(props, 0);
}

static bool list_has_next(value props) {
  return props != Val_emptylist;
}

static value list_tail(value props) {
  return Field(props, 1);
}

static char* get_prop_key(value prop) {
  return String_val(Field(prop, 0));
}

static value get_prop_value(value prop) {
  return Field(prop, 1);
}

template <class T>
class AbstractTranslator {
public:
  virtual T convert_string(char *str) = 0;
  virtual T convert_number(double n) = 0;
  virtual T convert_bool(long b) = 0;
  virtual T convert_null() = 0;
  virtual T convert_undefined() = 0;
  virtual T convert_object(value props) = 0;
  virtual T convert_array(value items) = 0;

  T convert_json(value v) {
    if (Is_long(v)) {
      if (Long_val(v) == 0) {
        return convert_null();
      }
      // no other immediate values should exist
      return convert_undefined(); // TODO: raise v8 exception
    };
    switch (Tag_val(v)) {
      // JObject
      case 0:
        return convert_object(Field(v, 0));

      // JArray
      case 1:
        return convert_array(Field(v, 0));

      // JString
      case 2:
        return convert_string(String_val(Field(v, 0)));

      // JNumber
      case 3:
        return convert_number(Double_val(Field(v, 0)));

      // JBool
      case 4:
        return convert_bool(Long_val(Field(v, 0)));

      default:
        // no other tags exist!
        return convert_undefined(); // TODO: raise v8 exception
    }
  }

  T parse(const char *content) {
    CAMLparam0();
    CAMLlocal2(content_val, result_val);

    static value * func = NULL;
    if (func == NULL) {
      func = caml_named_value("flow_parse");
    }

    content_val = caml_copy_string(content);
    result_val = caml_callback(*func, content_val);

    CAMLreturnT(T, convert_object(Field(result_val, 0)));
  }
};

void init() {
  char *argv[] = {NULL};
  caml_startup(argv);
}

} // namespace flow
