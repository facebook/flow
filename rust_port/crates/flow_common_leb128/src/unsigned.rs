/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

fn read_helper<F>(read_byte: &mut F, result: usize, shift: usize) -> usize
where
    F: FnMut() -> u8,
{
    let byte = read_byte();
    let result = result | (((byte & 0x7F) as usize) << shift);
    if byte & 0x80 == 0 {
        result
    } else {
        read_helper(read_byte, result, shift + 7)
    }
}

fn write_helper<F>(write_byte: &mut F, mut i: usize)
where
    F: FnMut(u8),
{
    let byte = (i & 0x7F) as u8;
    i >>= 7;
    if i != 0 {
        write_byte(byte | 0x80);
        write_helper(write_byte, i);
    } else {
        write_byte(byte);
    }
}

pub fn read<F>(read_byte: &mut F) -> usize
where
    F: FnMut() -> u8,
{
    read_helper(read_byte, 0, 0)
}

pub fn write<F>(write_byte: &mut F, i: usize)
where
    F: FnMut(u8),
{
    write_helper(write_byte, i);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_roundtrip(case: usize) {
        let mut buf = Vec::new();
        write(&mut |byte| buf.push(byte), case);

        let mut pos = 0;
        let out = read(&mut || {
            let byte = buf[pos];
            pos += 1;
            byte
        });

        assert_eq!(out, case);
    }

    #[test]
    fn roundtrip_0() {
        test_roundtrip(0);
    }

    #[test]
    fn roundtrip_127() {
        test_roundtrip(127);
    }

    #[test]
    fn roundtrip_128() {
        test_roundtrip(128);
    }

    #[test]
    fn roundtrip_255() {
        test_roundtrip(255);
    }

    #[test]
    fn roundtrip_256() {
        test_roundtrip(256);
    }

    #[test]
    fn roundtrip_1024() {
        test_roundtrip(1024);
    }

    #[test]
    fn roundtrip_max() {
        test_roundtrip(usize::MAX);
    }
}
