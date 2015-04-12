package pl.japila.scalania.s99

object S99_P31 {
  implicit class IntWithIsPrime(n: Int) {
    def isPrime: Boolean = {
      if(n < 2) {
        false
      } else {
        val potentialDivisors = List.range(2, (n / 2))
        potentialDivisors.takeWhile(divisor => n % divisor != 0) == potentialDivisors
      }
    }
  }
}
