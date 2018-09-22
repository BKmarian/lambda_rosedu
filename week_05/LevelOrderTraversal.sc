import scala.collection.mutable.Queue
import scala.util.Try

class Node(val n: Int, var l: Option[Node] = None, var r: Option[Node] = None) {
  override def toString: String = s" # ${n} # "
}

def recur(runningQ: Queue[Node], resultQ: Queue[Int]): Unit = {
  val crtNodeOption = Try(runningQ.dequeue).toOption
  crtNodeOption.foreach { case crtNode =>
    List(crtNode.l, crtNode.r).flatten.foreach { case child => {
        runningQ.enqueue(child)
        resultQ.enqueue(child.n) }}
    recur(runningQ, resultQ)
  }
}

val bTwo = new Node(2)
val bOne = new Node(1)
val bThree = new Node(3)
val bFour = new Node(4)
val bSeven = new Node(7)

bTwo.l = Some(bOne)
bTwo.r = Some(bThree)
bOne.r = Some(bFour)
bThree.r = Some(bSeven)

def performLevelOrderTraversal(rootNodeOption: Option[Node]): List[Int] = {
  rootNodeOption.map { rootNode => {
    val runningQ: Queue[Node] = Queue(rootNode)
    val resultQ: Queue[Int] = Queue(rootNode.n)
    recur(runningQ, resultQ)
    resultQ.toList
  }}.getOrElse( List.empty[Int] )
}

val result = performLevelOrderTraversal( Some(bTwo) )
