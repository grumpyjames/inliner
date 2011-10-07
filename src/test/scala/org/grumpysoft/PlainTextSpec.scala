package org.grumpysoft

import org.specs2._
import java.io.File

class PlainTextSpec extends Specification {
  def is =
    "This is a specification to check the inliner can inline plain text"   ^
      p^
      "the result of inlining should"                                      ^
        "be the template with the specified content inlined"           ! e1^
        "inline recursively"                                           ! e2^
        "figure out the path of the referred file may be relative"     ! e3^
  end


  val template =
"""In computer programming, some oft used variable names are as follows:

!inline:src/test/data/inline_content.txt

In reality, more descriptive variable names are recommended"""
  val recursive_template =
"""In computer programming, some oft used variable names are as follows:

!inline:src/test/data/inliner_content_with_another_inline.txt

In reality, more descriptive variable names are recommended"""

  val inline_result =
"""In computer programming, some oft used variable names are as follows:

Foo
Bar

In reality, more descriptive variable names are recommended"""

  def inline(template: String) : String = {
    val lines = template.lines
    lines.map(doReplacements _).reduceLeft(_ + "\n" + _)
  }

  def defaultFileFinder: String => File = {
    fileName => FileFinder.file(fileName)
  }

  def fileAsLines(file: File) = {
    scala.io.Source.fromFile(file).getLines
  }

  def inline(templateFile: File) : String = {
    fileAsLines(defaultFileFinder(templateFile.getPath))
      .map(doReplacements(_, FileFinder.relativeFinder(templateFile)))
      .reduceLeft(_ + "\n" + _)
  }

  def doReplacements(line: String) : String = {
    doReplacements(line, defaultFileFinder)
  }

  def doReplacements(line: String, fileFinder: String => File) : String = {
    if (line.startsWith("!inline")) {
      val splitByColon : Array[String] = line.split(':')
      val toInline = fileFinder(splitByColon(1))
      fileAsLines(toInline).map(doReplacements(_, FileFinder.relativeFinder(toInline))).reduceLeft(_ + "\n" + _)
    } else {
      line
    }
  }

  def converted = inline(template)
  def recursive_inline = inline(recursive_template)
  def converted_from_file = inline(new File("src/test/data/inliner_template.txt"))

  def e1 = converted must_== inline_result
  def e2 = recursive_inline must_== inline_result
  def e3 = converted_from_file must_== inline_result
}
