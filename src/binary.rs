pub trait BinaryOperator {
    fn binary_starts_with(&self, pattern: u8) -> bool;
}

impl BinaryOperator for u8 {
    fn binary_starts_with(&self, pattern: u8) -> bool {
        let comparator = *self >> pattern.leading_zeros();
        comparator == pattern
    }
}

pub fn combined_u8(first: u8, second: u8) -> u16 {
    ((first as u16) << 8) | (second as u16)
}

#[cfg(test)]
pub fn split_u16_to_u8(value: u16) -> (u8, u8) {
    let high_byte = (value >> 8) as u8;
    let low_byte = (value & 0xFF) as u8;
    (high_byte, low_byte)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_instruction() {
        let input: u8 = 0b10001001;
        assert!(input.binary_starts_with(0b10));
        assert!(input.binary_starts_with(0b1));
        assert!(input.binary_starts_with(0b100));
        assert!(input.binary_starts_with(0b10001));
        assert!(!input.binary_starts_with(0b1111));
    }
}
