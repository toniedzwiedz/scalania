package pl.japila.scalania.s99

import scala.util.Random

object S99_P23 {
  type RandomSelectFn[T] = (Int, Seq[T]) => Seq[T]

  def solutions[T]: List[(String, RandomSelectFn[T])] = List(
    ("my own implementation", randomSelect),
    ("my own implementation based on permutations", randomSelectPerm),
    ("my own implementation based on permutations", randomSelectShuffle)
  )

  def randomSelect[T](count: Int, ts: Seq[T]): Seq[T] = {
    (count, ts) match  {
      case (0, _) => Nil
      case (_, Nil) => Nil
      case (i, seq) => {
        val indexToDelete = Random.nextInt(seq.size)
        val (nextSeqHead, nextSeqTail) = seq.splitAt(indexToDelete)
        ts(indexToDelete) +: randomSelect(i - 1, nextSeqHead ++ nextSeqTail.drop(1))
      }
    }
  }

  def randomSelectPerm[T](count: Int, ts: Seq[T]): Seq[T] = {
    val permutations = ts.permutations.toIndexedSeq
    permutations(Random.nextInt(permutations.size)).take(count)
  }

  def randomSelectShuffle[T](count: Int, ts: Seq[T]): Seq[T] = {
    Random.shuffle(ts).take(count)
  }
}
