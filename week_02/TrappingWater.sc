case class StackItem(item: Int, thenMax: Int)
case class MaxStack(items: List[StackItem])

def push(s: MaxStack, value: Int): MaxStack = {
  val topOption = s.items.headOption
  topOption.map { case StackItem(item , thenMax) =>
    val newMax = scala.math.max(item, thenMax)
    MaxStack(StackItem(value, newMax) :: s.items)
  }.getOrElse {
    MaxStack(List(StackItem(value, 0)))
  }
}

def pop(s: MaxStack): (Option[Int], MaxStack) = {
  val topOption = s.items.headOption
  topOption.map { case StackItem(item , _) =>
    val restStack = MaxStack(s.items.tail)
    (Some(item), MaxStack(s.items.tail))
  }.getOrElse {
    (None, s)
  }
}

def max(s: MaxStack): Option[Int] = {
  val topOption = s.items.headOption
  topOption.map { case StackItem(item , thenMax) =>
    scala.math.max(item, thenMax)
  }
}


case class State(leftStack: MaxStack, rightStack: MaxStack, elems: List[Int], runningSum: Int)

val elevationList = List[Int](0,1,0,2,1,0,1,3,2,1,2,1)
val reversedElevationList = elevationList.reverse
val emptyMaxStack = MaxStack(List.empty[StackItem])
val initialRightStack = reversedElevationList.foldLeft(emptyMaxStack)(push)

val initialState = State(emptyMaxStack, initialRightStack, elevationList, 0)

def transition(s: State): State = {
  val restHeadOption = s.elems.headOption
  restHeadOption.map { case restHead =>
    val nextRest = s.elems.tail

    val leftMax = max(s.leftStack).getOrElse(0)
    val rightMax = max(s.rightStack).getOrElse(0)

    val waterLevel = scala.math.min(leftMax, rightMax) - restHead
    val positiveWaterLevel = if (waterLevel > 0) waterLevel else 0

    val updatedRunningSum = s.runningSum + positiveWaterLevel
    val newLeftStack = push(s.leftStack, restHead)
    val (_, newRightStack) = pop(s.rightStack)
    State(newLeftStack, newRightStack, nextRest, updatedRunningSum)
  }.getOrElse{s}
}

val trappedWater = {
  Stream
    .iterate(initialState)(transition)
    .dropWhile { case State(_, _, elems, _) => elems.nonEmpty }
    .head
    .runningSum
}
