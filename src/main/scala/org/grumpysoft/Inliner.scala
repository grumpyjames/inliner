package org.grumpysoft

import parser.{Expression, CommandParser}
import runner.CommandRunner
import java.lang.String
import java.io.{FileInputStream, BufferedReader, InputStreamReader, File}
import util.parsing.input.CharSequenceReader
import sys.process.ProcessLogger

object Inliner {

  def main(args: Array[String]) {
    val reader = new BufferedReader(new InputStreamReader(System.in))
    var firstLine: String = reader.readLine

    if (firstLine != null && firstLine.startsWith("!dependencies ({")) {
      val dependencyResult = processDependencies(reader)
      if (dependencyResult > 0) {
        System.err.println("dependency build failed - refusing to inline")
        System.exit(1)
      }
      firstLine = reader.readLine
    }

    while (firstLine != null) {
      System.out.println(doReplacements(firstLine, defaultFileFinder))
      firstLine = reader.readLine
    }
  }

  import FileFinder.relativeFinder
  import InlineExpressionParser.parse

  type Finder = String => File

  private val parser: CommandParser = new CommandParser()
  private val runner: CommandRunner = new CommandRunner()

  def processDependencies(reader: BufferedReader) : Int = {
    val dependencies = readDependency(reader).toList
    dependencies.foldLeft(0) { (exitCode: Int, expressions: List[Expression]) =>
      if (exitCode > 0) exitCode
      else runner.run(expressions).!(ProcessLogger({data => System.err.println(data)}))
    }
  }

  private def readLine(reader: BufferedReader) : (String, BufferedReader) = {
    (reader.readLine, reader)
  }

  // there should be a better way of returning the reader from this function.
  private def readDependency(reader: BufferedReader) : (Stream[List[Expression]]) = {
    val (line, newReader) = readLine(reader)
    if (line == null || line.startsWith("})")) {
      (Stream.empty[List[Expression]])
    } else {
      val list: List[Expression] = parser.fullyParse(line)
      (Stream.cons(list, { readDependency(newReader) }))
    }
  }

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
    val process = runner.run(parser.fullyParse(command))

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


