package main.scala

import main.scala.Utils.{MutableAlignmentParameters, loopThroughFiles}

object DefaultAlignmentParams {

  def getDefaultAlignmentParams(lang1FilePath: String,
                                lang2FilePath: String): MutableAlignmentParameters = {

    val alignmentParams = new collection.mutable.HashMap[(Int, Int, Int, Int), Double]
    val sentenceLengthPairs = collection.mutable.Set[(Int, Int)]()

    println("Initializing alignmentParams:")

    loopThroughFiles(lang1FilePath, lang2FilePath) {
      (line1, line2, index) =>
        sentenceLengthPairs += ((line1 split " ").size -> (line2 split " ").size)
    }

    val size = sentenceLengthPairs.size
    var index = 1
    sentenceLengthPairs foreach {
      case (l, m) =>
        printf("\r%3d%%", math.round((index * 100.0) / size))
        index += 1
        0 to l foreach (j =>
          1 to m foreach (i =>
            alignmentParams((j, i, l, m)) = 1.0 / (l + 1)))
    }

    println("Done Initializing alignmentParams")

    println("number of 4-tuples in alignmentParams:" + alignmentParams.size)

    alignmentParams
  }

}