struct fraction {
  int numerator;
  int denominator;
};
struct fraction fraction(const int numerator, const int denominator) {
  return (struct fraction) { numerator, denominator };
}
