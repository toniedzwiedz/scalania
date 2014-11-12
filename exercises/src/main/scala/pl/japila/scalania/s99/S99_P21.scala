package pl.japila.scalania.s99

object S99_P21 {

  type InsertAtFn = (Any, Int, Seq[Any]) => Seq[Any]

  val solutions: List[(String, InsertAtFn)] = List(
    ("my own implementation", insertAt),
    ("my own implementation", insertAtLists)
  )

  def insertAt[T](toAdd: T, position: Int, ts: Seq[T]): Seq[Any] = {
    //splitAt handles illegal positions by returning empty lists on either side of the sequence
    val (firstHalf, secondHalf) = ts.splitAt(position)
    firstHalf ++ (toAdd +: secondHalf)
  }
  def insertAtLists[T](toAdd: T, position: Int, ts: Seq[T]): Seq[Any] = {
        val (firstHalf, secondHalf) = ts.toList.splitAt(position)
        firstHalf ::: toAdd :: secondHalf
  }
}
