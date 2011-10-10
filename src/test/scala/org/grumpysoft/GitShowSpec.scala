package org.grumpysoft

import org.specs2.Specification

class GitShowSpec extends Specification {
  import Inliner.inline

  def is =
    "This specifies the behaviour of inline git templates" ^ p^
      "When inlining a git template" ^
        "The content should be from the correct file and revision"   ! e1 ^
        "Further plain text inlines should work from that file, too" ! e2 ^
    end

  val result = "First version"
  val template = "!inline:git+src/test/data/many_versioned_file.txt@428de6"

  val resultWithNestedInline =
  """Another version
Foo
Bar"""
  val templateWithNestedInline = "!inline:git+src/test/data/many_versioned_file.txt@13f151"


  def e1 = inline(template) must_==result
  def e2 = inline(templateWithNestedInline) must_==resultWithNestedInline
}