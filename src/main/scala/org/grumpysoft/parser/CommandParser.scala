package org.grumpysoft.parser

import scala.util.parsing.combinator._
import util.matching.Regex
import util.parsing.input.CharSequenceReader

sealed trait Expression {
  def show() : String
}

case class Fragment(shard: String) extends Expression {
  def show() = shard
}

object Pipe extends Expression {
  def show() = "|"
}

object RedirectInput extends Expression {
  def show() = ">"
}

class CommandParser extends JavaTokenParsers {

  def pipe: Parser[Expression] = "|" ^^ {_ => Pipe}
  def redirectInput: Parser[Expression] = "<" ^^ {_ => RedirectInput}
  def literally: Parser[Expression] = regex(("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r) ^^ {s => new Fragment(s)}
  def fragment: Parser[Expression] = regex(new Regex("[^\\s|^\\||^>]+")) ^^ {s => new Fragment(s)}

  def commandParser: Parser[Expression] = pipe | redirectInput | literally | fragment

  def fullyParse(in: CharSequenceReader) : Stream[ParseResult[Expression]] = {
    showNextParsed(in)
  }

  def showNextParsed(in: Input) : Stream[ParseResult[Expression]] = {
    if(in.atEnd) {
      Stream.empty[ParseResult[Expression]]
    }
    else {
      val result: ParseResult[Expression] = commandParser.apply(in)
      Stream.cons(result, showNextParsed(result.next))
    }
  }
}