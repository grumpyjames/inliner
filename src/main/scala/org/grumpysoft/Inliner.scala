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

  def doReplacements(line: String, fileFinder: String => File) : String = {
    if (line.startsWith("!inline")) {
      val splitByColon = line.split(':')
      val inlineInvocation = splitByColon(1)
      val splitByPlus = line.split('+')
      if (splitByPlus.size > 1) {
        doGitReplacement(splitByPlus(1))
      } else {
        val toInline = fileFinder(inlineInvocation)
        fileAsLines(toInline).map(doReplacements(_, FileFinder.relativeFinder(toInline))).reduceLeft(_ + "\n" + _)
      }
    } else {
      line
    }
  }
}


