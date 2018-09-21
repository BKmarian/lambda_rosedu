val xs = List(List(1, 1), 2, List(1, 1))

def parseLevel(depth: Int)(input: Any): Int = {
  input match {
    case i: Int => i * depth
    case l: List[Any] => l.map(parseLevel(depth + 1)).sum
  }
}

def getResult(input: Any): Int = {
  input
  .asInstanceOf[List[Any]]
  .map(parseLevel(1))
  .sum
}

val result = getResult(xs)

def parseLevel2(depth: Int)(input: Any): Seq[Any] = {
  input match {
    case i: Int => List(i * depth)
    case l: List[Any] => l.flatMap(parseLevel2(depth + 1))
  }
}

val result2 = xs.flatMap(parseLevel2(1)).asInstanceOf[List[Int]].sum
