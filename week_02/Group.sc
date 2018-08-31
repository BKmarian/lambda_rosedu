import scala.collection.immutable.Queue

case class State(resultQ: Queue[String], rest: List[Char])

val inputString = "Hellooo woooorld"

val initialState = State(Queue.empty[String], inputString.toList)

def transition(s: State): State = {
  s.rest.headOption.map { case remHead =>
    val (fst, snd) = s.rest.span(_ == remHead)
    val nextResultQ = s.resultQ.enqueue(fst.mkString)
    State(nextResultQ, snd)
  }.getOrElse(s)
}

val solution = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile(_.rest.nonEmpty)
    .head
    .resultQ
    .toList
}
