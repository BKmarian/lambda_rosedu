case class State(fst: Int, snd: Int)

def transition(arr: Array[Int], arrLen: Int)(s: State): State = {
  (arr(s.fst) == 0, arr(s.snd) == 0) match {
    case (true, true) => s.copy(snd=math.min(s.snd+1,arrLen-1))
    case (true, false) => {
      arr(s.fst) = arr(s.snd)
      arr(s.snd) = 0
      State(s.fst+1, math.min(s.snd+1,arrLen-1))
    }
    case _ => State(s.fst+1, math.min(s.snd+1,arrLen-1))
  }
}

val arr = Array(1,0,0,2,2,2,3,3,3,3,4,4)

val arrLen = arr.length

val initialState = State(0,1)

Stream.iterate(initialState)(transition(arr, arrLen)).dropWhile(_.snd < arrLen - 1).tail.head

val result = arr.mkString
