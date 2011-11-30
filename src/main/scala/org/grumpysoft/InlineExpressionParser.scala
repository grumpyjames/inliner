package org.grumpysoft

case class ParseResult(inlineExpr: String,  leftOver: String)

object InlineExpressionParser {
  def parse(line: String) : ParseResult = {
    val lastCloser = findLastCloserIndex(line)
    ParseResult(line.substring(0, lastCloser), line.substring(lastCloser + 1, line.length))
  }

  def findLastCloserIndex(line: String) : Int = {
    var opened = 1
    for (i <- 0 until line.length) {
      line.charAt(i) match {
        case ')' => opened-=1
        case '(' => opened+=1
        case _ =>
      }
      if (opened == 0)
        return i
    }
    -1
  }
}



