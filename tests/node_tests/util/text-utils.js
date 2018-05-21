import { TextDecoder, TextEncoder } from 'util';

// it supports the basic API
{
  const encoder = new TextEncoder();
  const uint8array = encoder.encode('this is some data');
  (uint8array: Uint8Array);

  const decoder = new TextDecoder();
  const result = decoder.decode(uint8array);
  (result: string);
}
