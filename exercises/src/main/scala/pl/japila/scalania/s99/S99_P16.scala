package pl.japila.scalania.s99

object S99_P16 {
  def drop[T](n: Int, ts: Seq[T]): Seq[T] = {

    def dropNth[T](which: Int, seq: Seq[T]) : Seq[T] = {

      if (seq.isEmpty) {
        Nil
      } else if (which == 1) {
        dropNth(n, seq.tail)
      } else {
        seq.head +: dropNth(which - 1, seq.tail)
      }
    }

    dropNth(n, ts)
  }
}
