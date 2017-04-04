def last[T](list: List[T]): T = list match {
  case x::Nil => x
  case _ => last(list.tail)
}

def penultimate[T](list: List[T]): T = list match {
  case x::_::Nil => x
  case _ => penultimate(list.tail)
}

def nth[T](n: Int, list: List[T]): T = list match {
  case h::t if (n == 0) => h
  case _ => nth(n - 1, list.tail)
}

def length[T](list: List[T]): Int = list match {
  case Nil => 0
  case x::t => 1 + length(list.tail)
}

def reverse[T](list: List[T]): List[T] = {
  list.foldLeft(Nil: List[T])((acc, a) => a :: acc)
}

def isPalindrome[T](list: List[T]): Boolean = {
  list == (reverse(list))
}

def flatten(list: List[Any]): List[Any] = list flatMap {
  case list: List[_] => flatten(list)
  case x => List(x)
}

def compress[T](list: List[T]): List[T] = list match {
  case Nil => Nil
  case x::t => x :: compress(t.dropWhile(_ == x))
}

def pack[T](list: List[T]): List[List[T]] = list match {
  case Nil => Nil
  case x::t => list.takeWhile(_ == x) :: pack(list.dropWhile(_ == x))
}

def encode[T](list: List[T]): List[(Int, T)] = {
  pack(list).map(x => (x.length, x.head))
}

def make[T](n: Int, s: T): List[T] = n match {
  case 0 => Nil
  case _ => s :: make(n - 1, s)
}

def encodeModified[T](list: List[T]): List[Any] = {
  pack(list).map(x => if(x.length < 2) x.head else (x.length, x.head))
}

def decode[T](list: List[(Int, T)]): List[T] = {
  list.flatMap(x => make(x._1, x._2))
}

def duplicate[T](list: List[T]) : List[T] = list match {
  case Nil => Nil
  case x::t => make(list.takeWhile(_ == x).length * 2, x) ::: duplicate(list.dropWhile(_ == x))
}














