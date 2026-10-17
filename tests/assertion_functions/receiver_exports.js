export class ExportedMessage {
  assertText(): asserts this is ExportedTextMessage {
    if (!(this instanceof ExportedTextMessage)) throw new Error();
  }
}

export class ExportedTextMessage extends ExportedMessage {
  text: string = 'hello';
}
