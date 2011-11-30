package org.grumpysoft

import java.io.File
import scala.sys.process.Process
import java.lang.String
import annotation.tailrec

object Inliner {
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
    replaceThenJoin(Process(command).lines, defaultFileFinder).stripLineEnd
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
            fileAsLines(toInline).map(doReplacements(_, FileFinder.relativeFinder(toInline))).reduceLeft(_ + "\n" + _) +
            doReplacements(parseResult.leftOver, fileFinder)
        } else {
          line.substring(0, a) + doCmdLineInvoc(parseResult.inlineExpr) + doReplacements(parseResult.leftOver, fileFinder)
        }
      }
    }


  }
}


