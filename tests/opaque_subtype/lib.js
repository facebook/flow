// $FlowExpectedError[libdef-override]
declare opaque type React$Element<+C,+P=any>: {...};
// $FlowExpectedError[libdef-override]
declare opaque type React$Node super React$Element<any> | string | void;
