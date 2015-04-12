package pl.japila.scalania.s99

object S99_P32 {
  def gcd(m: Int, n: Int): Int = {
    if (n == 0) m else gcd(n, m % n)
  }
}
