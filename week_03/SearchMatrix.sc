import scala.collection.Searching.{search, Found, InsertionPoint}

var lookedUp: Long = 845L

val matr: Vector[Vector[Long]] =
  Vector(Vector(148, 185, 238, 256, 291, 314, 324),
         Vector(350, 356, 367, 382, 383, 414, 418),
         Vector(432, 446, 507, 511, 527, 540, 552),
         Vector(553, 556, 592, 607, 610, 632, 634),
         Vector(661, 675, 691, 698, 704, 723, 738),
         Vector(739, 767, 788, 813, 816, 842, 847),
         Vector(851, 853, 947, 972, 974, 986, 990))

def searchMatrix(m: Vector[Vector[Long]], k: Long): Boolean = {
  val firsts = matr.map { x => x(0) }
  val possibleRowIndex = firsts.search(k)
  possibleRowIndex match {
    case Found(_) => true
    case InsertionPoint(0) => false
    case InsertionPoint(i) => matr(i).search(k).isInstanceOf[Found]
  }
}

val result = searchMatrix(matr, lookedUp)
