package org.grumpysoft

import org.specs2.mutable._

class ResourceFinderSpec extends Specification {
  import FileFinder._

  "The relative file finder" should {
    "find files relative to the original file" in {
      relativeFinder(file("src/test/data/inline_content.txt"))("inliner_result.txt") must_==file("src/test/data/inliner_result.txt")
    }
  }
}