// @flow

declare const objProto: typeof Object.__proto__;
declare const funProto: typeof Function.__proto__;
declare const funProtoApply: typeof Function.prototype.apply;
declare const funProtoBind: typeof Function.prototype.bind;
declare const funProtoCall: typeof Function.prototype.call;


export function foo() {
    return {
        objProto,
        funProto,
        funProtoApply,
        funProtoBind,
        funProtoCall,
    }
}
