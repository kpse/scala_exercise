object Main {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = true

  def main(args: Array[String]): Unit = {
    val sorted: Boolean = isSorted(Array(1,2,3), (a: Int, b: Int) => a < b)
    print(s"sorted = ${sorted}")
  }
}
