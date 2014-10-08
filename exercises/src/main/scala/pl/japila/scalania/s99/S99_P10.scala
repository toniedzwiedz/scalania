package pl.japila.scalania.s99

object S99_P10 {
  def encode[T](ts: Seq[T]): Seq[(Int, T)] = {
    if (ts == Nil || ts.length < 1) {
      Nil
    } else {
      val tailGroup = ts.dropWhile(elem => elem == ts.head)
      val headGroupLength = ts.length - tailGroup.length
      (headGroupLength, ts.head) +: encode(tailGroup)
    }
  }
}
