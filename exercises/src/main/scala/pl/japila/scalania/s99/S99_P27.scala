package pl.japila.scalania.s99

object S99_P27 {
  type GroupFn[T] = Seq[T] => Seq[Seq[Seq[T]]]
  type GroupGeneralizedFn[T] = (Seq[Int], Seq[T]) => Seq[Seq[Seq[T]]]

  def solutions[T]: List[(String, GroupFn[T])] = List(
    ("my own implementation", group[T])
  )

  def solutionsGen[T](): List[(String, GroupGeneralizedFn[T])] = List(
    ("my own implementation", groupGeneralized[T])
  )

  def group[T](ts: Seq[T]): Seq[Seq[Seq[T]]] = {
//    import scala.collection.mutable.Seq
    val (firstGroupSize, secondGroupSize, lastGroupSize) = (4, 3, 2)
    var result: Seq[Seq[Seq[T]]] = Seq()
    ts.combinations(firstGroupSize).foreach((combinationOfFour: Seq[T]) => {
      val remainingAfter4: Seq[T] = ts diff combinationOfFour
      remainingAfter4.combinations(secondGroupSize).foreach((combinationOfThree: Seq[T]) => {
        val remainingPair: Seq[T] = ts diff (combinationOfFour union combinationOfThree)
        require(remainingPair.length == lastGroupSize)
        result = Seq(combinationOfFour, combinationOfThree, remainingPair) +: result
      })
    })
    result
  }

  def groupGeneralized[T](groups: Seq[Int], ts: Seq[T]): Seq[Seq[Seq[T]]] = {

    def buildSolution[T](remainingGroups : Seq[Int], remainingElements : Seq[T]) : Seq[Seq[T]] = {
      (remainingGroups, remainingElements) match {
        case (Seq(), _) => Seq()
        case (Seq(_), remainingElements) => { //last group
          require(remainingElements.length == remainingGroups.head)
          Seq(remainingElements)
        }
        case (_, remainingElements) => {
          var result : Seq[Seq[T]] = Seq()
          val currentGroupSize = remainingGroups.head
          remainingElements.combinations(currentGroupSize).foreach((combinationOfSize : Seq[T]) => {
            val nextGroup : Seq[Seq[T]] = buildSolution(remainingGroups.tail, remainingElements diff combinationOfSize)
            result = combinationOfSize +: nextGroup
          })
          result
        }
      }
    }

    require(ts.length == groups.reduce((a, b) => a + b))
    (groups, ts) match {
      case (Seq(), _) => Seq()
      case (Seq(_), ts) => { //last group, stop condition
        require(ts.length == groups.head)
        Seq(Seq(ts))
      }
      case (_, ts) => {
        var result : Seq[Seq[Seq[T]]] = Seq()
        val currentGroupSize = groups.head
        ts.combinations(currentGroupSize).foreach((combinationOfSize : Seq[T]) => {
          result = result :+ (combinationOfSize +: buildSolution(groups.tail, ts diff combinationOfSize))
        })
        result
      }
    }
  }
}
