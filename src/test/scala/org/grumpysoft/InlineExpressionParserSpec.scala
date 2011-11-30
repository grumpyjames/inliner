package org.grumpysoft

import org.specs2.mutable._

class InlineExpressionParserSpec extends Specification {
  import InlineExpressionParser.parse

  "the inliner parser" should {
    "return the content until the correct brace" in {
      parse("foo)").inlineExpr must_== "foo"
    }
    "not get fooled by other parentheses being opened and closed" in {
      parse("sometimes (i.e all the time) I need parentheses)").inlineExpr must_== "sometimes (i.e all the time) I need parentheses"
    }
    "return the unparsed piece of the line" in {
      parse("short)after expression").leftOver must_=="after expression"
    }
  }

}