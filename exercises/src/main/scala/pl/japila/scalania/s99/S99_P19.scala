package pl.japila.scalania.s99

object S99_P19 {
  def rotate[T](n: Int, ts: Seq[T]): Seq[T] = {
    if(n == 0) {
      ts
    } else if (n > 0) {
      ts.drop(n) ++ ts.take(n)
    } else {
      ts.takeRight(-n) ++ ts.dropRight(-n)
    }
  }
}
