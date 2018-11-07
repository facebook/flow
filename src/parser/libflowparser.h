/**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE

#include <map>
#include <string>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

namespace flowparser {

typedef std::map<std::string, int> options_t;

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

value cons(value hd, value tl) {
  CAMLparam2(hd, tl);
  CAMLlocal1(ret);
  ret = caml_alloc(2, 0);
  Store_field(ret, 0, hd);
  Store_field(ret, 1, tl);
  CAMLreturn(ret);
}

template <class T>
class AbstractTranslator {
public:
  virtual ~AbstractTranslator() = default;
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

  T parse(const char *content, options_t opts) {
    CAMLparam0();
    CAMLlocal4(content_val, option_val, options_val, result_val);

    static value * func = caml_named_value("flow_parse");

    content_val = caml_copy_string(content);

    options_val = Val_int(0); // empty list
    for (auto& x : opts) {
      option_val = caml_alloc_tuple(2);
      Store_field(option_val, 0, caml_copy_string(x.first.c_str()));
      Store_field(option_val, 1, Val_bool(x.second));
      options_val = cons(option_val, options_val);
    }

    result_val = caml_callback2(*func, content_val, options_val);

    CAMLreturnT(T, convert_object(Field(result_val, 0)));
  }
};

void init() {
  char *argv[] = {NULL};
  caml_startup(argv);
}

} // namespace flow
