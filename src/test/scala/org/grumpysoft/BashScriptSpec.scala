package org.grumpysoft

import org.specs2.Specification

class BashScriptSpec extends Specification {
  import Inliner.inline

  def is =
    "This specifies the behaviour of inlining bash scripts" ^ p^
      "When inlining a bash script" ^
      "The content should be the stdout of the bash script"   ! e1 ^
      "Any inlines in that content must also be inlined"      ! e2 ^
  end

  val expectedOutput = "foo, bar, baz"
  val inlineTemplate = """foo, !inline:bash+`echo bar`, baz"""
  val inlineTemplateWithAnotherInline = """foo, !inline:bash+`echo !inline:src/test/data/just_bar.txt`, baz"""

  def e1 = inline(inlineTemplate) must_==expectedOutput
  def e2 = inline(inlineTemplateWithAnotherInline) must_==expectedOutput
}