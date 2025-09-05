// $FlowExpectedError[libdef-override]
declare opaque type ExactReactElement_DEPRECATED<+C,+P=any>: {...};
// $FlowExpectedError[libdef-override]
declare opaque type React$Node super ExactReactElement_DEPRECATED<any> | string | void;
