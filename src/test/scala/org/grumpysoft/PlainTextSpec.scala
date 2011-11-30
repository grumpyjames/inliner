package org.grumpysoft

import org.specs2._
import java.io.File

class PlainTextSpec extends Specification {
  import Inliner.inline
  def is =
    "This is a specification to check the inliner can inline plain text"   ^
      p^
      "the result of inlining should"                                      ^
        "be the template with the specified content inlined"           ! e1^
        "inline recursively"                                           ! e2^
        "figure out the path of the referred file may be relative"     ! e3^
        "from any part of a line"                                      ! e4^
  end


  val template =
"""In computer programming, some oft used variable names are as follows:

!inline(file://src/test/data/inline_content.txt)

In reality, more descriptive variable names are recommended"""
  val recursive_template =
"""In computer programming, some oft used variable names are as follows:

!inline(file://src/test/data/inliner_content_with_another_inline.txt)

In reality, more descriptive variable names are recommended"""

  val inline_result =
"""In computer programming, some oft used variable names are as follows:

Foo
Bar

In reality, more descriptive variable names are recommended"""



  def converted = inline(template)
  def recursive_inline = inline(recursive_template)
  def converted_from_file = inline(new File("src/test/data/inliner_template.txt"))
  def inline_with_midline_invocation = inline(new File("src/test/data/midway_inline.txt"))

  def e1 = converted must_== inline_result
  def e2 = recursive_inline must_== inline_result
  def e3 = converted_from_file must_== inline_result
  def e4 = inline_with_midline_invocation must_== inline_result
}
