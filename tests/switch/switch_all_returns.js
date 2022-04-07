// @flow

function switchOnTypeAllReturn(action: {|type: 1|} | {|type: 2|}): void {
  switch (action.type) {
    case 1:
      return;
    case 2:
      return;
    default:
      return;
  }
}
