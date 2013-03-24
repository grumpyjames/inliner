package org.grumpysoft

import parser.CommandParser
import runner.CommandRunner
import java.lang.String
import java.io.{FileInputStream, BufferedReader, InputStreamReader, File}
import util.parsing.input.CharSequenceReader

object Inliner {
  def main(args: Array[String]) {
    val converter = new InputStreamReader(System.in)
    val reader = new BufferedReader(converter)
    var thisLine: String = reader.readLine
    while (thisLine != null) {
      System.out.println(doReplacements(thisLine, defaultFileFinder))
      thisLine = reader.readLine
    }
  }

  import FileFinder.relativeFinder
  import InlineExpressionParser.parse

  type Finder = String => File

  def inline(template: String) : String = {
    replaceThenJoin(template.lines, defaultFileFinder)
  }

  def inline(templateFile: File) : String = {
    replaceThenJoin(fileAsLines(defaultFileFinder(templateFile.getPath)), relativeFinder(templateFile))
  }

  private def defaultFileFinder : Finder = {
    fileName => FileFinder.file(fileName)
  }

  private def fileAsLines(file: File) : Iterator[String] = {
    scala.io.Source.fromFile(file).getLines()
  }

  private def doCmdLineInvoc(command: String) : String = {
    val parseResult = new CommandParser().fullyParse(new CharSequenceReader(command))
    val process = new CommandRunner().run(parseResult.map(_.get).toList)

    replaceThenJoin(process.lines, defaultFileFinder).stripLineEnd
  }

  private def replaceThenJoin(lines: TraversableOnce[String], finder: Finder) : String = {
    lines.map(doReplacements(_, finder)).reduceLeft(_ + "\n" + _)
  }

  private def doReplacements(line: String, fileFinder: Finder) : String = {
    line.indexOf("!inline(") match {
      case -1 => line
      case a: Int => {
        val parseResult = parse(line.substring(a + 8))
        if (parseResult.inlineExpr.startsWith("file://")) {
          val toInline = fileFinder(parseResult.inlineExpr.substring(7))
          line.substring(0, a) +
            replaceThenJoin(fileAsLines(toInline), FileFinder.relativeFinder(toInline)) +
            doReplacements(parseResult.leftOver, fileFinder)
        } else {
          line.substring(0, a) + doCmdLineInvoc(parseResult.inlineExpr) + doReplacements(parseResult.leftOver, fileFinder)
        }
      }
    }


  }
}


