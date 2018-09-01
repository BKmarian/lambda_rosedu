import scala.util.Random.nextInt

case class State(seq: List[Int], length: Int, k: Int) {
  override def toString = s"""${this.seq.mkString("[",",","]")} // ${this.length} // ${this.k}"""
}

def trimHeadDuplicates(arr: List[Int]): List[Int] = {
  arr.take(2) match {
    case List(i,j) if (i == j) => arr.tail
    case _ => arr
  }
}

def transition(s: State): State = {
  val pivotIndex = nextInt(s.length)
  val pivotValue = s.seq(pivotIndex)
  val (tmpLeftPartition, tmpRightPartition) = s.seq.partition(_ < pivotValue)
  val leftPartition = trimHeadDuplicates(tmpLeftPartition)
  val rightPartition = trimHeadDuplicates(tmpRightPartition)
  val leftPartitionLength = leftPartition.length
  val rightPartitionLength = rightPartition.length

  if (s.k <= leftPartitionLength) {
    State(leftPartition, leftPartitionLength, s.k)
  } else {
    State(rightPartition, rightPartitionLength, s.k - leftPartitionLength)
  }
}

val inputList = List(883, 521, 162, 826, 523, 461, 421, 425, 170, 468, 957, 902, 467, 613, 177, 462, 376, 883, 217, 856, 119)

val initialState = State(inputList, inputList.length, 18)

// Stream.iterate(initialState)(transition).take(20).foreach(println)

val kthElement = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile(_.length > 1)
    .head
    .seq
    .head
}
