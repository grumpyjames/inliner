package org.grumpysoft

import java.io.File
import scala.sys.process.Process
import java.lang.String

object Inliner {
  import FileFinder.relativeFinder

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

  private def doGitReplacement(gitCmd: String) : String = {
    // assumes: . is a git directory
    // format like src/test/data/many_versioned_file@428de6
    val fileAndVersion = gitCmd.split("@")
    val filePath: String = fileAndVersion(0)
    val showInvocation: String = "git show " + fileAndVersion(1) + ":" + filePath
    // annoyingly git ends with a newline, so we have to strip it.
    replaceThenJoin(Process(showInvocation).lines, relativeFinder(new File(filePath))).stripLineEnd
  }

  private def doBashReplacement(bashCmd: String, finder: Finder) : String = {
    val delimitedCmd: Array[String] = bashCmd.split('`')
    val shellCmd = delimitedCmd(1)
    replaceThenJoin(Process(shellCmd).lines, finder) + delimitedCmd(2)
  }

  private def replaceThenJoin(lines: TraversableOnce[String], finder: Finder) : String = {
    lines.map(doReplacements(_, finder)).reduceLeft(_ + "\n" + _)
  }

  private def doReplacements(line: String, fileFinder: Finder) : String = {
    val splitByColon = line.split("!inline:")
    if (splitByColon.size == 1) return line

    val splitByPlus = line.split('+')
    if (splitByPlus.size == 1) {
      val toInline = fileFinder(splitByColon(1))
      return splitByColon(0) + fileAsLines(toInline).map(doReplacements(_, FileFinder.relativeFinder(toInline))).reduceLeft(_ + "\n" + _)
    }

    val invocationType: String = splitByPlus(0).split(':')(1)
    invocationType match {
      case "git" => splitByColon(0) + doGitReplacement(splitByPlus(1))
      case "bash" => splitByColon(0) + doBashReplacement(splitByPlus(1), fileFinder)
    }
  }
}


