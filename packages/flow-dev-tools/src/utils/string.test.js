import {splitIntoChunks} from './string';

test('splitIntoChunks', () => {
  expect(splitIntoChunks('', 1)).toEqual([]);
  expect(splitIntoChunks('abcd', 2)).toEqual(['ab', 'cd']);
  expect(splitIntoChunks('abc', 2)).toEqual(['ab', 'c']);
  expect(splitIntoChunks('abc', 10)).toEqual(['abc']);
  expect(splitIntoChunks('abc', 1)).toEqual(['a', 'b', 'c']);
  // The check marks are multi-byte characters when encoded with UTF-8. Make sure they are treated
  // as single characters and not split up into individual bytes.
  expect(splitIntoChunks('✓✓✓✓✓', 1)).toEqual(['✓', '✓', '✓', '✓', '✓']);
});
