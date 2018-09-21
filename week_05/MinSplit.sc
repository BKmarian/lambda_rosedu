import scala.collection.immutable.Queue

case class Acc(remString: String, q: Queue[String], qLength: Int)

def recur(dict: List[String])(acc: Acc): Seq[Acc] = {
  if (acc.remString.nonEmpty) {
    dict
    .filter { dictWord => acc.remString.startsWith(dictWord) }
    .map { dictWord => Acc(acc.remString.stripPrefix(dictWord), acc.q.enqueue(dictWord), acc.qLength + 1) }
    .flatMap(recur(dict))
  } else Seq(acc)
}

def minSplit(word: String, dict: List[String]): List[String] = {
  val initialAcc = Acc(word, Queue.empty[String], 0)
  recur(dict)(initialAcc)
  .sortBy(_.qLength)
  .headOption
  .map(_.q.toList)
  .getOrElse(List.empty[String])
}

val word = "jumpedoversomething"
val dict = List("jump", "jumped", "jumpedov", "over", "some", "thing", "something")

val result = minSplit(word, dict)

/////////////////////////////////////////////////////////////////////////////////

def recur2(distinctLengthList: List[Int])(acc: Acc): Seq[Acc] = {
  if (acc.remString.nonEmpty) {
    distinctLengthList.map(acc.remString.take)
    .filter { word => dict.contains(word) }
    .map { dictWord => Acc(acc.remString.stripPrefix(dictWord), acc.q.enqueue(dictWord), acc.qLength + 1) }
    .flatMap(recur2(distinctLengthList))
  } else Seq(acc)
}

def minSplit2(word: String, distinctLengthList: List[Int]): List[String] = {
  val initialAcc = Acc(word, Queue.empty[String], 0)
  recur2(distinctLengthList)(initialAcc)
  .sortBy(_.qLength)
  .headOption
  .map(_.q.toList)
  .getOrElse(List.empty[String])
}

val word2 = "jumpedoversomething"

val dict2 = List("jump", "jumped", "jumpedov", "over", "some", "thing", "something")
val distinctLengthList: List[Int] = dict2.map(_.length).distinct

val result2 = minSplit2(word2, distinctLengthList)
