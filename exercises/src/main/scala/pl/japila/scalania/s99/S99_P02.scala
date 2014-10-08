package pl.japila.scalania.s99

object S99_P02 {
  def penultimate[T](ts: Seq[T]): Option[T] = {
    ts.take(ts.length-1).lastOption
   }
}
