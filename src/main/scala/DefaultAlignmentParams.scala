package main.scala

import Utils.AlignmentParameters
import Utils.loopThroughFiles

trait DefaultAlignmentParams {

  def getDefaultAlignmentParams(lang1FilePath: String, lang2FilePath: String) = {

    val alignmentParams = new AlignmentParameters
    val sentenceLengthPairs = collection.mutable.Set[(Int, Int)]()

    println("Initializing alignmentParams:")

    loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

      sentenceLengthPairs += ((line1 split " ").size -> (line2 split " ").size)

    })

    sentenceLengthPairs foreach {
      case (l, m) =>
        println(".") // TODO change? (horizontal bar)
        0 to l foreach (j =>
          1 to m foreach (i =>
            alignmentParams((j, i, l, m)) = 1.0 / (l + 1)))
    }

    println("Done Initializing alignmentParams")

    println("number of 4-tuples in alignmentParams:" + alignmentParams.size)

    alignmentParams
  }

}