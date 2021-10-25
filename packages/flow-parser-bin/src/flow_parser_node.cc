/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <flowparser/libflowparser.h>
#include <nan.h>

using namespace std;
using namespace v8;
using namespace flowparser;

class V8Translator : public AbstractTranslator<Local<Value>> {
 public:
  Local<Value> convert_string(char* str) {
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
      Nan::Set(
          obj,
          convert_string(get_prop_key(prop)),
          convert_json(get_prop_value(prop)));
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
      Nan::Set(arr, i, convert_json(head));
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
  Nan::Utf8String arg(info[0]);
  std::string content(*arg);

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
          Local<Value> key = Nan::Get(props, i).ToLocalChecked();
          Local<Value> value = Nan::Get(opts, key).ToLocalChecked();
          Nan::Utf8String name(key);
          options.insert(std::pair<std::string, int>(
              std::string(*name), Nan::To<bool>(value).FromJust()));
        }
      }
    }
  }

  Local<Value> result = converter.parse(content.c_str(), options);
  info.GetReturnValue().Set(result);
}

NAN_MODULE_INIT(Init) {
  flowparser::init();
  Nan::Set(
      target,
      Nan::New("parse").ToLocalChecked(),
      Nan::GetFunction(Nan::New<v8::FunctionTemplate>(Parse)).ToLocalChecked());
}

NODE_MODULE(flow_parser, Init)
