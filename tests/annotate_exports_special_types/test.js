// @flow

declare var objProto: typeof Object.__proto__;
declare var funProto: typeof Function.__proto__;
declare var funProtoApply: typeof Function.prototype.apply;
declare var funProtoBind: typeof Function.prototype.bind;
declare var funProtoCall: typeof Function.prototype.call;


export function foo() {
    return {
        objProto,
        funProto,
        funProtoApply,
        funProtoBind,
        funProtoCall,
    }
}
