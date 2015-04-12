package pl.japila.scalania.euler

object Euler_P01 {

  val solutions = List[(Int, Int, Int) => Int](
    findSumOfMultiplies
  )

  def findSumOfMultiplies: (Int, Int, Int) => Int = (a: Int, b: Int, limit: Int) => {
    var result = 0
    for (i <- 0 until limit) {
      if (i % 3 == 0 || i % 5 == 0) {
        result += i;
      }
    }
    result
  }

}
