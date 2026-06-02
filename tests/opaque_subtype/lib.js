// $FlowExpectedError[libdef-override]
declare opaque type ExactReactElement_DEPRECATED<out C,out P=any>: {...};
// $FlowExpectedError[libdef-override]
declare opaque type React$Node super ExactReactElement_DEPRECATED<any> | string | void;
