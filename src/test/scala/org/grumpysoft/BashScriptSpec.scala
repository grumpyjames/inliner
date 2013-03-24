package org.grumpysoft

import org.specs2.Specification

class BashScriptSpec extends Specification {
  import Inliner.inline

  def is =
    "This specifies the behaviour of inlining bash scripts" ^ p^
      "When inlining a bash script" ^
      "The content should be the stdout of the bash script"   ! e1 ^
      "Any inlines in that content must also be inlined"      ! e2 ^
      "Can have two distinct inlines within the same line"    ! e3 ^
      "Can pipe within an inline"                             ! e4 ^
      "Can deal with quoted pipes in inline"                  ! e5 ^
      "Can use sed to pick lines out of a file"               ! e6 ^
      "Can use sed from path to pick lines out of a file"     ! e7 ^
  end

  val expectedOutput = "foo, bar, baz"
  val inlineTemplate = "foo, !inline(echo bar), baz"
  val inlineTemplateWithAnotherInline = "foo, !inline(echo !inline(file://src/test/data/just_bar.txt)), baz"
  val twoInlines = "foo, !inline(echo bar), baz, !inline(echo quux)"
  val inlineWithPipe = "foo, !inline(echo bar | grep bar), baz"
  val echoWithPipeInside = "foo !inline(echo \"bar|, baz\")"
  val sedWithFileSource = "!inline(/bin/sed -n 3,5p < src/test/data/file_of_line_numbers.txt)"
  val executableInPathStillWorks = "!inline(sed -n 3,5p < src/test/data/file_of_line_numbers.txt)"

  def e1 = inline(inlineTemplate) must_==expectedOutput
  def e2 = inline(inlineTemplateWithAnotherInline) must_==expectedOutput
  def e3 = inline(twoInlines) must_=="foo, bar, baz, quux"
  def e4 = inline(inlineWithPipe) must_==expectedOutput
  def e5 = inline(echoWithPipeInside) must_=="foo \"bar|, baz\""
  def e6 = inline(sedWithFileSource) must_=="3\n4\n5"
  def e7 = inline(executableInPathStillWorks) must_=="3\n4\n5"
}