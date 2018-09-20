val a = List(4, 3, 2, 1, 5)
val b = List(0, 1, 0, 0, 0)

case class Fish(size: Int, direction: Int)
case class SolState(fishstack: List[Int], numAlive: Int)
case class FightState(fishstack: List[Int], fishSize: Option[Int])

def afterFighting(fs: FightState): FightState = {
  def transition(s: FightState): FightState = {
    (fs.fishstack.headOption, fs.fishSize) match {
      case (Some(fishStackHead), Some(fishSize)) =>
        if (fishStackHead > fishSize) {
          FightState(fs.fishstack, None)
        } else {
          FightState(fs.fishstack.tail, fs.fishSize)
        }

      case _ => fs
    }
  }
  
  Stream
    .iterate(fs)(transition)
    .dropWhile { case FightState(fishstack, fishSize) =>
      fishstack.nonEmpty && fishSize.isDefined }
    .head
}

/*
  put all downstream fishes in a stack
  any upstream fish has to fight all fishes on the stack
    => if there is no fish on the stack, the fish survives
  if the stack has some downstream fishes at the end, they also survives
*/

// TODO: needs optimizations on codility
def solution(a: List[Int], b: List[Int]): Int = {
  val fishes = a.zip(b).map { case (x: Int, y: Int) => Fish(x, y) }
  val initialSolState = SolState(List[Int](), 0)

  def foldLeftFn(s: SolState, crtFish: Fish): SolState = {
    if (crtFish.direction == 1) {
      SolState(crtFish.size :: s.fishstack, s.numAlive)
    } else {
      val initialFightState = FightState(s.fishstack, Some(crtFish.size))
      val afterFightState = afterFighting(initialFightState)
      if (afterFightState.fishstack.isEmpty) {
        SolState(afterFightState.fishstack, s.numAlive + 1)
      } else {
        SolState(afterFightState.fishstack, s.numAlive)
      } 
    }
  }
    
  val finalSolState = fishes.foldLeft(initialSolState)(foldLeftFn)
  val totalFishAlive = finalSolState.fishstack.length + finalSolState.numAlive

  totalFishAlive
}

val result = solution(a, b)
