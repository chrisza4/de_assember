pub trait BinaryOperator {
    fn binary_starts_with(&self, pattern: u8) -> bool;
}

impl BinaryOperator for u8 {
    fn binary_starts_with(&self, pattern: u8) -> bool {
        let comparator = *self >> pattern.leading_zeros();
        comparator == pattern
    }
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
