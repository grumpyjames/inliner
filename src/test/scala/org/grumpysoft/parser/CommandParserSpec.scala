package org.grumpysoft.parser

import org.specs2.mutable.Specification
import util.parsing.input.CharSequenceReader


class CommandParserSpec extends Specification {
  "The parser" should {
    "parse" in {
      val in: CharSequenceReader = new CharSequenceReader("foo | bar")
      val results = new CommandParser().fullyParse(in).map(_.get).toSeq
      results must_==(Seq(new Fragment("foo"), Pipe, new Fragment("bar")))
    }
    "deal with string literals" in {
      val in: CharSequenceReader = new CharSequenceReader("foo | bar \"|||\"")
      val results = new CommandParser().fullyParse(in).map(_.get).toSeq
      results must_==(Seq(new Fragment("foo"), Pipe, new Fragment("bar"), new Fragment("\"|||\"")))
    }
  }
}