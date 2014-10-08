package pl.japila.scalania.s99

object S99_P07 {
  def flatten(ls: Seq[Any]): Seq[Any] = ls flatMap {
    case i: List[_] => flatten(i)
    case e: Any => List(e)
  }
}
