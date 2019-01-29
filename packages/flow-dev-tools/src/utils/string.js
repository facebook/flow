/* @flow */

export function splitIntoChunks(str: string, chunkSize: number): Array<string> {
  let result = [];
  for (let i = 0; i < str.length; i += chunkSize) {
    result.push(str.substr(i, chunkSize));
  }
  return result;
}
