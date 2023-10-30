object Main extends App {
}

def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
  val tmp = arr(i)
  arr(i) = arr(j)
  arr(j) = tmp
}

def joinByComma(start: Int, end: Int): String = {
  (start to end).mkString(",")
}

def listReverse[T](list: List[T]) : List[T] = {
  list.foldLeft(List.empty[T])((x, y) => y :: x).reverse
}