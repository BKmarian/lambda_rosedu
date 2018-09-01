import scala.collection.Searching.{search, Found}

def sumOfTwo(a: List[Int], b: List[Int], v: Int): Boolean = {
  if (a.isEmpty || b.isEmpty) false
  else {
    val aVector = a.toVector
    b.exists { n => aVector.search(v-n).isInstanceOf[Found]}
  }
}

val a = List(10, 20, 30, 40)
val b = List(5,  15, 25)
val v = 64

val result = sumOfTwo(a, b, v)
