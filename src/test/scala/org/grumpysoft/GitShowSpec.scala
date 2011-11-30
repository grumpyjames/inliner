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
  val template = "!inline(git show 428de6:src/test/data/many_versioned_file.txt)"

  val resultWithNestedInline =
  """Another version
Foo
Bar"""
  val templateWithNestedInline = "!inline(git show 999d6a:src/test/data/many_versioned_file.txt)"


  def e1 = inline(template) must_==result
  def e2 = inline(templateWithNestedInline) must_==resultWithNestedInline
}