import {ExportedMessage} from './receiver_exports';

class Message {
  assertText(): asserts this is TextMessage {
    if (!(this instanceof TextMessage)) throw new Error();
  }

  assertTextAgain(): asserts this is TextMessage {
    this.assertText();
  }

  viaThisReceiver() {
    this.assertText();
    this as TextMessage;
    this.text as string;
    this.text as number; // ERROR
  }

  viaArrowReceiver(): () => void {
    return () => {
      this.assertText();
      this.text as string;
      this.text as number; // ERROR
    };
  }
}

class MoreMessage extends Message {
  viaSuperReceiver() {
    super.assertText();
    this as TextMessage;
    this.text as string;
    this.text as number; // ERROR
  }
}

class TextMessage extends MoreMessage {
  text: string = 'hello';
}

function objectReceiver(message: Message) {
  message.assertText();
  message as TextMessage;
  message.text as string;
  message.text as number; // ERROR
}

function nestedReceiver(holder: {message: Message}) {
  holder.message.assertText();
  holder.message as TextMessage;
  holder.message.text as string;
  holder.message.text as number; // ERROR
}

function receiverExpressionPosition(message: Message) {
  const result = message.assertText();
  result as void;
  message as TextMessage; // ERROR
}

function receiverAssignment(message: Message) {
  message.assertText();
  message = new Message();
  message as TextMessage; // ERROR
}

class ReceiverWithArguments {
  assertText(ignored: unknown): asserts this is ReceiverWithText {
    if (!(this instanceof ReceiverWithText)) throw new Error();
  }
}

class ReceiverWithText extends ReceiverWithArguments {
  text: string = 'hello';
}

function receiverArgumentRead(message: ReceiverWithArguments) {
  message.assertText(message);
  message.text as string;
  message.text as number; // ERROR
}

function receiverArgumentWrite(message: ReceiverWithArguments) {
  message.assertText(message = new ReceiverWithArguments());
  message as ReceiverWithText; // ERROR: argument replaced the receiver binding
}

function receiverPropertyArgumentWrite(holder: {message: ReceiverWithArguments}) {
  holder.message.assertText(holder.message = new ReceiverWithArguments());
  holder.message as ReceiverWithText; // ERROR
}

function receiverArgumentCall(message: ReceiverWithArguments, effect: () => void) {
  message.assertText(effect());
  message.text as string;
  message.text as number; // ERROR
}

function receiverPropertyArgumentCall(holder: {message: ReceiverWithArguments}, effect: () => void) {
  holder.message.assertText(effect());
  holder.message as ReceiverWithText; // ERROR: call can replace the property
}

function receiverSpread(message: ReceiverWithArguments, args: Array<unknown>) {
  message.assertText(...args);
  message.text as string;
  message.text as number; // ERROR
}

interface HasValue {
  readonly value: string;
}

declare class Box {
  value: ?string;
  assertHasValue(): asserts this is HasValue;
}

function structuralReceiver(box: Box) {
  box.assertHasValue();
  box.value as string;
  box.value as null; // ERROR
}

function localReceiver() {
  const message: Message = new Message();
  message.assertText();
  message.text as string;
  message.text as number; // ERROR
}


function structuralReceiverWrite(box: Box) {
  box.assertHasValue();
  box.value = null;
  box.value as string; // ERROR
}


declare class GenericMessage<T> {
  assertTyped(): asserts this is TypedMessage<T>;
}

declare class TypedMessage<T> extends GenericMessage<T> {
  value: T;
}

function genericReceiver(message: GenericMessage<number>) {
  message.assertTyped();
  message.value as number;
  message.value as string; // ERROR
}

function importedReceiver(message: ExportedMessage) {
  message.assertText();
  message.text as string;
  message.text as number; // ERROR
}

function receiverLoop(messages: Array<Message>) {
  for (const message: Message of messages) {
    message.assertText();
    message.text as string;
    message.text as number; // ERROR
  }
}

function receiverComma(first: Message, second: Message) {
  first.assertText(), second.assertText();
  first.text as string;
  second.text as string;
  second.text as number; // ERROR
}
