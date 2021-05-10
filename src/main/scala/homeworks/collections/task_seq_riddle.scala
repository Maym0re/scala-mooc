package homeworks.collections

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    currentLine.foldLeft(List[Int]())((acc: List[Int], item: Int) => {
      item match {
        case _ if acc.isEmpty => 1 :: item :: Nil
        case item if item == acc.last => acc.slice(0, acc.size - 2) :+ (acc.takeRight(2).head + 1) :+ acc.last
        case item if item != acc.last => acc :+ 1 :+ item
      }
    })
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = {
    def createLazyList(list: List[Int]): LazyList[List[Int]] = LazyList.cons(list, createLazyList(nextLine(list)))
    createLazyList(List(1))
  }
}
