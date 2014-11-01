package pl.japila.scalania.s99

import scala.collection.mutable.ListBuffer

object S99_P12 {
  def decode[T](its: Seq[(Int, T)]): Seq[T] = {
    val result = new ListBuffer[T]()
    its.foreach({ pair =>
      for (i <- 1 to pair._1) {
        result.append(pair._2)
      }
    })
    result.toList
  }
}
