package org.grumpysoft

import java.io.File

object FileFinder {
  def relativeFinder(baseFile: File)(childPath: String) : File = {
    val path = baseFile.getParentFile.getPath + "/" + childPath
    file(path)
  }

  def file(path: String) = new File(path)
}

