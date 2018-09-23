/*
Given an encoded string, return its corresponding decoded string.
The encoding rule is: k[encoded_string],
where the encoded_string inside the square brackets is repeated exactly k times.
Note: k is guaranteed to be a positive integer.
Note that your solution should have linear complexity because this is what you will be asked during an interview.
For s = "4[ab]", the output should be
decodeString(s) = "abababab"
For s = "2[b3[a]]", the output should be
decodeString(s) = "baaabaaa"
For s = "z1[y]zzz2[abc]", the output should be
decodeString(s) = "zyzzzabcabc"
*/

import scala.util.parsing.combinator.RegexParsers

trait AST
case class LetterSeq(value: String) extends AST
case class IntLiteral(value: String) extends AST
case class Repeater(count: IntLiteral, content: List[AST]) extends AST

class ExprParser extends RegexParsers {
  def intLiteral: Parser[AST] = "[0-9]+".r ^^ IntLiteral
  def letterSeq: Parser[AST] = "[a-z]+".r ^^ LetterSeq
  def term: Parser[AST] = letterSeq | repeater
  def expr: Parser[List[AST]] = rep1(term)
  def repeater: Parser[AST] = intLiteral ~ ("[" ~> expr <~ "]") ^^ {
    case intLiteral ~ expr =>
      Repeater(intLiteral.asInstanceOf[IntLiteral], expr)
  }
}

def stringify(ast: AST): String = {
  ast match {
    case LetterSeq(str) => str
    case Repeater(IntLiteral(multiplier), expr) =>
      expr.map(stringify).mkString * multiplier.toInt
  }
}

def decodeString(s: String): String = {
  s match {
    case "" => ""
    case nonEmptyString => {
      val p = new ExprParser
      p.parseAll(p.expr, nonEmptyString).get.map(stringify).mkString
    }
  }
}
