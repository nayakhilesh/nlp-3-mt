package alignments

import scala.Array.canBuildFrom
import scala.compat.Platform

class IbmModel2(initialTranslationParams: collection.mutable.Map[String, collection.mutable.Map[String, Double]],
    initialAlignmentParams: collection.mutable.Map[String, Double]) {

  val translationParams = initialTranslationParams

  def initialize(lang1FilePath: String, lang2FilePath: String, numIterations: Int) {

    val startEm = Platform.currentTime

    val temp = translationParams.toSeq.flatMap {
      case (word1, map) =>
        map.foldLeft(List[(String, String)]()) { case (list, (word2, _)) => (word1, word2) +: list }
    }

    println("number of lang2|lang1 combinations in translationParams:" + temp.size)

    1 to numIterations foreach { iter =>
      println("Starting iteration #" + iter)

      val c1 = collection.mutable.Map[String, Double]()
      val c2 = collection.mutable.Map[(String, String), Double]()

      loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

        line2 split " " foreach { word2 =>

          val denom = ((line1 split " ") :+ NULL).foldLeft(0.0)((acc, word1) => acc + translationParams(word1)(word2))
          (line1 split " ") :+ NULL foreach { word1 =>
            val delta = translationParams(word1)(word2) / denom
            c2((word1, word2)) = c2.getOrElse((word1, word2), 0.0) + delta
            c1(word1) = c1.getOrElse(word1, 0.0) + delta
          }

        }

      })

      temp foreach {
        case (word1, word2) =>
          translationParams(word1)(word2) = c2((word1, word2)) / c1(word1)
      }

      println("Finished iteration #" + iter)
    }

    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    val endEm = Platform.currentTime

    println("EM time=" + (endEm - startEm) / 1000.0 + "s")

  }

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String) {

    val outputFile = new java.io.FileWriter(outputFilePath)

    loopThroughFiles(input1FilePath, input2FilePath)((line1: String, line2: String, index: Int) => {

      for ((word2, index2) <- line2 split " " zipWithIndex) {

        val (_, maxIndex) = ((NULL +: (line1 split " ")) zipWithIndex).maxBy {
          case (word1, index1) => translationParams(word1)(word2)
        }
        outputFile.write((index + 1) + " " + maxIndex + " " + index2 + "\n")

      }

    })

    outputFile.close()

  }

}