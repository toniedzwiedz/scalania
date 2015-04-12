package pl.japila.scalania.s99

object S99_P28 {
  type LsortFn[T] = Seq[Seq[T]] => Seq[Seq[T]]
  type LsortFreqFn[T] = Seq[Seq[T]] => Seq[Seq[T]]

  def solutions[T]: List[(String, LsortFn[T])] = List(
    ("my own implementation", lsort[T]))

  def solutionsFreq[T]: List[(String, LsortFreqFn[T])] = List(
    ("my own implementation", lsortFreq[T])
  )

  def lsort[T](ts: Seq[Seq[T]]): Seq[Seq[T]] = {
    ts.sortWith((lt : Seq[T], lt2 : Seq[T]) => {
      lt.length < lt2.length
    })
  }

  def lsortFreq[T](tss: Seq[Seq[T]]): Seq[Seq[T]] = {

    type KVP = (Int, Seq[Seq[T]])

    def compareByNumberOfEntries(kvp1 : KVP, kvp2 : KVP) : Boolean = {
      kvp1._2.length < kvp2._2.length
    }

    val groupedByLength = tss.groupBy(_.length)
    val sortedKeyValuePairs : Seq[KVP] = groupedByLength.toList.sortWith(compareByNumberOfEntries)
    sortedKeyValuePairs.map(pair => pair._2).flatten
  }
}
