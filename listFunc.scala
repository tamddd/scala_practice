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

def sum(list: List[Int]): Int = {
  list.foldLeft(0)((x, y) => x + y)
}

def mul(list: List[Int]): Int = {
  list.foldLeft(1)((x, y) => x * y)
}

def myMkString[T](list: List[T])(sep: String): String = list match {
  case Nil => ""
  case head::next => next.foldLeft(head.toString)((x, y) => x + sep + y)
}

def myMap[T, U](list: List[T])(f: T => U): List[U] = {
  list.foldLeft(Nil: List[U])((x, y) => f(y) :: x).reverse
}

def myFilter[T](list: List[T])(f: T => Boolean): List[T] = {
  list.foldLeft(Nil: List[T])((x, y) => if (f(y)) y:: x else x).reverse
}

def myFindRec[T](list: List[T])(f: T => Boolean): Option[T] = {
  list match {
    case Nil => None
    case head::next => if (f(head)) Some(head) else myFindRec(next)(f)
  }
}

def myTakeWhile[T](list: List[T])(f: T => Boolean): List[T] = {
  list match {
    case head::next if f(head) => head :: myTakeWhile(next)(f)
    case _ => Nil
  }
}

def myCount[T](list: List[T])(f: T => Boolean): Int = {
  list.foldLeft(0)((x, y) => if (f(y)) x + 1 else x)
}

/*
解説
List(1, 2, 3)をflatにしている、それをMapに渡している
Mapの中身の関数は、eを引数にして(中身はList(1, 2, 3）が入ってくる)
それをList(4, 5)のmapに渡している
そのmapの中身の関数はgを引数にして、先ほどのeと掛け算
List(1, 2, 3).flatMap{e => List(4, 5).map(g => e * g)}
 */

def myFlatMap[T, U](list: List[T])(f: T => List[U]): List[U] = {
  list match {
    case head::next => f(head) ::: myFlatMap(next)(f)
    case Nil => List.empty[U]
  }
}
