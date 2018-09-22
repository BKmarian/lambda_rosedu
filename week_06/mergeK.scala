import collection.{mutable => mut}
import util.Try

case class State(result: mut.Queue[Int], pq: mut.PriorityQueue[(Int,Int)], remData: Array[List[Int]])

val data = Array( List(21,22,43), List(31,32,33), List(41,42,44) )

val leftColumn = data
.zipWithIndex
.map { case (lst, i) => (lst.headOption, i) }
.collect { case (Some(n),i) => (n,i) }

val pq = mut.PriorityQueue[(Int,Int)](leftColumn: _*)(Ordering.Tuple2[Int,Int].reverse)

val initialData = data.map { case lst => if (lst.nonEmpty) lst.tail else List() }

val initialState = State(mut.Queue.empty[Int], pq, initialData)

def transition(s: State): State = {
  Try(s.pq.dequeue).toOption.foreach { case (n, listIndex) =>
    s.result.enqueue(n)
    s.remData(listIndex).headOption.foreach { case newN =>
      {
        s.pq.enqueue( (newN, listIndex) )
        s.remData(listIndex) = s.remData(listIndex).tail
      }
    }
  }
  s
}

val result = Stream
  .iterate(initialState)(transition)
  .dropWhile(_.pq.nonEmpty)
  .head
  .result
  .toList
