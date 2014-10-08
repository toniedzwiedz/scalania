package pl.japila.scalania.s99


object S99_P08 {
  def compress[T](ts: Seq[T]): Seq[T] = {
    if( ts == Nil || ts.length < 1) {
      Nil
    } else if (ts.length == 1) {
        ts
    } else if (ts.tail.head == ts.head) {
        compress(ts.tail)
    } else {
        ts.head +: compress(ts.tail)
    }
  }
}
