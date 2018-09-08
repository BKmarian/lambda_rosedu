import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.{Map => MMap}

class SWItem(val v: Int, var expired: Boolean = false) extends Ordered[SWItem] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: SWItem): Int = this.v compare that.v
  override def toString: String = s"Item(${v}, ${expired})"
}

case class SlidingWindow(pq: PriorityQueue[SWItem], hm: MMap[Int, SWItem])

val inputList: List[Int] = List(-7, 1, 5, 2, -4, 3, 0, -1, -7, -9, -8)

val slidingWindowSize: Int = 3

val tupleList: List[(Int,Int)] = inputList.zipWithIndex

val firstBatch: List[(Int,Int)] = tupleList.take(slidingWindowSize)

val restOfList: List[(Int,Int)] = tupleList.drop(slidingWindowSize)

val emptySlidingWindow: SlidingWindow = SlidingWindow(PriorityQueue.empty[SWItem](Ordering.by(_.v)), MMap.empty[Int, SWItem])

val initialSlidingWindow: SlidingWindow = firstBatch.foldLeft(emptySlidingWindow) { case (accSlidingWindow, tpl) => {
  val (v,i) = tpl
  val newItem: SWItem = new SWItem(v)
  accSlidingWindow.pq.enqueue(newItem)
  accSlidingWindow.hm(i) = newItem
  accSlidingWindow
}}

val firstMax = initialSlidingWindow.pq.head.v

restOfList.zipWithIndex.foreach { case ((v,rightIndex),leftIndex) => {
  println(s"v = ${v} | leftIndex = ${leftIndex} | rightIndex = ${rightIndex}")
}}

val result = restOfList.zipWithIndex.scanLeft((initialSlidingWindow, firstMax)) { case ((accSlidingWindow,cMax),tpl) => {
  val ((v,rightIndex),leftIndex) = tpl
  val newItem: SWItem = new SWItem(v)
  accSlidingWindow.pq.enqueue(newItem)
  accSlidingWindow.hm(rightIndex) = newItem
  accSlidingWindow.hm(leftIndex).expired = true
  val lastState = Stream.iterate(accSlidingWindow) { sw => { if (sw.pq.head.expired) { sw.pq.dequeue; sw } else sw }}.dropWhile(_.pq.head.expired).head
  println(lastState == accSlidingWindow)
  println(lastState)
  println(accSlidingWindow)
  (lastState, lastState.pq.head.v)
}}.map(_._2)
