// adapted from the Rust standard library:
// https://github.com/rust-lang/rust/blob/9d1b2106e23b1abd32fce1f17267604a5102f57a/library/core/src/str/validations.rs

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Checks whether the byte is a UTF-8 continuation byte (i.e., starts with the
/// bits `10`).
#[inline]
const fn utf8_is_cont_byte(byte: u8) -> bool {
    (byte as i8) < -64
}

/// Reads the next code point out of a byte iterator (assuming a
/// UTF-8-like encoding).
// TODO #[inline]
pub fn next_code_point<'a, I: Iterator<Item = &'a u8>>(bytes: &mut I) -> Option<char> {
    // Decode UTF-8
    let x = *bytes.next()?;
    if x < 128 {
        return Some(x as char);
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    let y = *bytes.next()?;
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        let z = *bytes.next()?;
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            let w = *bytes.next()?;
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    // TODO: is ch always a valid char by construction?
    // can we use char::from_u32_unchecked?
    char::from_u32(ch)
}

/// Mask of the value bits of a continuation byte.
const CONT_MASK: u8 = 0b0011_1111;
