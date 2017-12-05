/**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <nan.h>
#include <flowparser/libflowparser.h>

using namespace std;
using namespace v8;
using namespace flowparser;

class V8Translator : public AbstractTranslator<Local<Value>> {
public:
  Local<Value> convert_string(char *str) {
    return Nan::New(str).ToLocalChecked();
  };
  Local<Value> convert_number(double n) {
    return Nan::New(n);
  }
  Local<Value> convert_bool(long b) {
    return Nan::New<Boolean>(b);
  }
  Local<Value> convert_null() {
    return Nan::Null();
  }
  Local<Value> convert_undefined() {
    return Nan::Undefined();
  }
  Local<Value> convert_object(value props) {
    Local<Object> obj = Nan::New<Object>();
    value prop;

    while (list_has_next(props)) {
      prop = list_head(props);
      Nan::Set(obj,
        convert_string(get_prop_key(prop)),
        convert_json(get_prop_value(prop))
      );
      props = list_tail(props);
    }

    return obj;
  }

  Local<Value> convert_array(value items) {
    Local<Array> arr = Nan::New<Array>();
    value head;
    size_t i = 0;
    while (list_has_next(items)) {
      head = list_head(items);
      arr->Set(i, convert_json(head));
      items = list_tail(items);
      i++;
    }
    return arr;
  }
};

NAN_METHOD(parse) {
  Nan::HandleScope scope;
  V8Translator converter;
  String::Utf8Value arg(info[0]);
  string content = string(*arg);
  Local<Value> result = converter.parse(content.c_str());
  info.GetReturnValue().Set(result);
}

NAN_MODULE_INIT(Init) {
  flowparser::init();
  NAN_EXPORT(target, parse);
}

NODE_MODULE(flow_parser, Init)
