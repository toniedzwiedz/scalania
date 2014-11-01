package pl.japila.scalania.s99

object S99_P11 {
  def encodeModified[T](ts: Seq[T]): Seq[Either[(Int, T), T]] = {
    if ( ts == Nil ) {
      Nil
    } else {
      val tailGroup = ts.dropWhile(elem => elem == ts.head)
      val headGroupLength = ts.length - tailGroup.length
      if (headGroupLength > 1) {
        Left((headGroupLength, ts.head)) +: encodeModified(tailGroup)
      } else {
        Right(ts.head) +: encodeModified(tailGroup)
      }
    }
  }
}
