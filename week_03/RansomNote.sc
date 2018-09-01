val newspaperText = "in while attending the university of helsinki torvalds became curious about operating systems frustrated by the licensing of minix which at the time limited it to educational use only he began to work on his own operating system kernel which eventually became the linux kernel".replaceAll("[^a-z]", "")
val targetText = "helloworld"

def countLetters(s: String): List[Int] = {
  var mutarr = Array.fill[Int](256)(0)
  s.foreach { case i => mutarr(i.toInt) += 1}
  mutarr.toList
}

def isPossible(target: String, newspaper: String): Boolean = {
  val countTarget = countLetters(target)
  val countNewsPaper = countLetters(newspaper)
  
  val zipped = countNewsPaper.zip(countTarget).map { case (a:Int, b:Int) => a - b}
  !zipped.exists(_ < 0)
}

val result = isPossible(targetText, newspaperText)
