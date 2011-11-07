package org.grumpysoft

import java.io.File
import scala.sys.process.Process

object Inliner {
  import FileFinder.relativeFinder

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
      .map(doReplacements(_, relativeFinder(templateFile)))
      .reduceLeft(_ + "\n" + _)
  }

  def doReplacements(line: String) : String = {
    doReplacements(line, defaultFileFinder)
  }

  def doGitReplacement(gitCmd: String) : String = {
    // assumes: . is a git directory
    // format like src/test/data/many_versioned_file@428de6
    val fileAndVersion = gitCmd.split("@")
    Process("git show " + fileAndVersion(1) + ":" + fileAndVersion(0))
      .lines.map(doReplacements(_, relativeFinder(new File(fileAndVersion(0))))).reduceLeft(_ + "\n" + _).stripLineEnd
  }

  def doBashReplacement(bashCmd: String, finder: String => File) : String = {
    val delimitedCmd: Array[String] = bashCmd.split('`')
    val shellCmd = delimitedCmd(1)
    Process(shellCmd).lines.map(doReplacements(_, finder)).reduceLeft(_ + "\n" + _) + delimitedCmd(2)
  }

  def doReplacements(line: String, fileFinder: String => File) : String = {
    val splitByColon = line.split("!inline:")
    if (splitByColon.size > 1) {
      val inlineInvocation = splitByColon(1)
      val splitByPlus = line.split('+')
      if (splitByPlus.size > 1) {
        val invocationType: String = splitByPlus(0).split(':')(1)
        invocationType match {
          case "git" => splitByColon(0) + doGitReplacement(splitByPlus(1))
          case "bash" => splitByColon(0) + doBashReplacement(splitByPlus(1), fileFinder)
        }

      } else {
        val toInline = fileFinder(inlineInvocation)
        splitByColon(0) + fileAsLines(toInline).map(doReplacements(_, FileFinder.relativeFinder(toInline))).reduceLeft(_ + "\n" + _)
      }
    } else {
      line
    }
  }
}


