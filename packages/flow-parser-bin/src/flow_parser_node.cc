/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <flowparser/libflowparser.h>
#include <napi.h>

using namespace flowparser;

class V8Translator : public AbstractTranslator<Napi::Value> {
  Napi::Env env;

public:
  V8Translator(Napi::Env env) : env(env) {}

  Napi::Value convert_string(const char* str) {
    return Napi::String::New(env, str);
  }

  Napi::Value convert_number(double n) {
    return Napi::Number::New(env, n);
  }

  Napi::Value convert_bool(long b) {
    return Napi::Boolean::New(env, static_cast<bool>(b));
  }

  Napi::Value convert_null() {
    return env.Null();
  }

  Napi::Value convert_undefined() {
    return env.Undefined();
  }

  Napi::Value convert_object(value props) {
    Napi::Object obj = Napi::Object::New(env);
    value prop;

    while (list_has_next(props)) {
      prop = list_head(props);
      Napi::Value key = convert_string(get_prop_key(prop));
      Napi::Value value = convert_json(get_prop_value(prop));
      obj.Set(key, value);
      props = list_tail(props);
    }

    return obj;
  }

  Napi::Value convert_array(value items) {
    Napi::Array arr = Napi::Array::New(env);
    value head;
    size_t i = 0;

    while (list_has_next(items)) {
      head = list_head(items);
      Napi::Value value = convert_json(head);
      arr.Set(i, value);
      items = list_tail(items);
      i++;
    }

    return arr;
  }
};

Napi::Value Parse(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();

  if (info.Length() < 1) {
    Napi::TypeError::New(env, "Wrong number of arguments").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  if (!info[0].IsString()) {
    Napi::TypeError::New(env, "First argument must be a string").ThrowAsJavaScriptException();
    return env.Undefined();
  }

  V8Translator converter(env);
  Napi::String arg = info[0].As<Napi::String>();
  std::string content = arg.Utf8Value();

  options_t options;

  if (info.Length() > 1 && info[1].IsObject()) {
    auto opts = info[1].As<Napi::Object>();
    auto propNames = opts.GetPropertyNames();
    auto propNamesLength = propNames.Length();

    for (uint32_t i = 0; i < propNamesLength; i++) {
      Napi::Value value = opts.Get(propNames.Get(i));
      
      options.insert(std::pair<std::string, int>(
          propNames.Get(i).ToString(), value.ToBoolean()));
    }
  }

  return converter.parse(content.c_str(), options);
}

Napi::Object Init(Napi::Env env, Napi::Object exports) {
  flowparser::init();

  exports.Set(Napi::String::New(env, "parse"), Napi::Function::New(env, Parse));

  return exports;
}

NODE_API_MODULE(flow_parser, Init)
