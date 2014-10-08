package pl.japila.scalania.s99

object S99_P09 {
  def pack[T](ts: Seq[T]): Seq[Seq[T]] = {
    if (ts == Nil || ts.length < 1) {
      Nil
    } else {
      val headGroup = ts.takeWhile(elem => elem == ts.head)
      val rest = ts.takeRight(ts.length - headGroup.length)
      headGroup +: pack(rest)
    }
  }
}
