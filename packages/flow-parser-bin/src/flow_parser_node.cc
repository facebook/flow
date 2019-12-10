/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

void Parse(const Nan::FunctionCallbackInfo<v8::Value>& info) {
  if (info.Length() < 1) {
    Nan::ThrowTypeError("Wrong number of arguments");
    return;
  }

  if (!info[0]->IsString()) {
    Nan::ThrowTypeError("First argument must be a string");
    return;
  }

  Nan::HandleScope scope;
  V8Translator converter;
  String::Utf8Value arg(info[0]);
  string content = string(*arg);

  options_t options;
  if (info.Length() > 1) {
    if (!info[1]->IsObject()) {
      Nan::ThrowTypeError("Second argument must be an object");
      return;
    } else {
      Local<Object> opts = Nan::To<Object>(info[1]).ToLocalChecked();
      Nan::MaybeLocal<Array> maybe_props = Nan::GetPropertyNames(opts);
      if (!maybe_props.IsEmpty()) {
        Local<Array> props = maybe_props.ToLocalChecked();
        for (uint32_t i = 0; i < props->Length(); i++) {
           Local<Value> key = props->Get(i);
           Local<Value> value = opts->Get(key);
           String::Utf8Value name(key);
           options.insert(std::pair<std::string, int>(
             string(*name),
             value->BooleanValue()
           ));
        }
      }
    }
  }

  Local<Value> result = converter.parse(content.c_str(), options);
  info.GetReturnValue().Set(result);
}

void Init(v8::Local<v8::Object> exports) {
  flowparser::init();
  exports->Set(Nan::New("parse").ToLocalChecked(),
               Nan::New<v8::FunctionTemplate>(Parse)->GetFunction());
}

NODE_MODULE(flow_parser, Init)
