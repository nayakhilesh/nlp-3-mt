package alignments

import scala.Array.canBuildFrom
import scala.compat.Platform
import scala.io.Source

class IbmModel2 {

  var translationParams = collection.mutable.Map[String, collection.mutable.Map[String, Double]]()
  var alignmentParams = collection.mutable.Map[(Int, Int, Int, Int), Double]()

  def computeParams(initialTranslationParams: collection.mutable.Map[String, collection.mutable.Map[String, Double]],
    initialAlignmentParams: collection.mutable.Map[(Int, Int, Int, Int), Double],
    lang1FilePath: String, lang2FilePath: String, numIterations: Int) {

    translationParams = initialTranslationParams
    alignmentParams = initialAlignmentParams

    val startEm = Platform.currentTime

    val temp = translationParams.toSeq.flatMap {
      case (word1, map) =>
        map.foldLeft(List[(String, String)]()) { case (list, (word2, _)) => (word1, word2) +: list }
    }

    val tempAlign = alignmentParams.keys

    println("number of lang2|lang1 combinations in translationParams:" + temp.size)
    println("number of 4-tuples in alignmentParams:" + tempAlign.size)

    1 to numIterations foreach { iter =>
      println("Starting iteration #" + iter)

      val c1 = collection.mutable.Map[String, Double]()
      val c2 = collection.mutable.Map[(String, String), Double]()
      val c3 = collection.mutable.Map[(Int, Int, Int), Double]()
      val c4 = collection.mutable.Map[(Int, Int, Int, Int), Double]()

      loopThroughFiles(lang1FilePath, lang2FilePath)((line1: String, line2: String, index: Int) => {

        val arr1 = line1 split " "
        val size1 = arr1.size
        val arr1WithIndex = NULL +: arr1 zipWithIndex

        val arr2WithIndex = line2 split " " zipWithIndex
        val size2 = arr2WithIndex.size

        arr2WithIndex foreach {
          case (word2, index2) =>

            val denom = (arr1WithIndex.foldLeft(0.0) {
              case (acc, (word1, index1)) =>
                acc + (alignmentParams((index1, index2 + 1, size1, size2)) * translationParams(word1)(word2))
            })
            arr1WithIndex foreach {
              case (word1, index1) =>
                val delta = (alignmentParams((index1, index2 + 1, size1, size2)) * translationParams(word1)(word2)) / denom
                c2((word1, word2)) = c2.getOrElse((word1, word2), 0.0) + delta
                c1(word1) = c1.getOrElse(word1, 0.0) + delta
                c4((index1, index2 + 1, size1, size2)) = c4.getOrElse((index1, index2 + 1, size1, size2), 0.0) + delta
                c3((index2 + 1, size1, size2)) = c3.getOrElse((index2 + 1, size1, size2), 0.0) + delta
            }

        }

      })

      temp foreach {
        case (word1, word2) =>
          translationParams(word1)(word2) = c2((word1, word2)) / c1(word1)
      }

      tempAlign foreach {
        case (index2, index1, size1, size2) => c4((index2, index1, size1, size2)) / c3((index1, size1, size2))
      }

      println("Finished iteration #" + iter)
    }

    println("number of lang1 words in translationParams:" + translationParams.size)
    println("number of lang2|lang1 combinations in translationParams:" +
      translationParams.foldLeft(0) { case (acc, (_, map)) => acc + map.size })

    println("number of 4-tuples in alignmentParams:" + alignmentParams.size)

    val endEm = Platform.currentTime

    println("EM time=" + (endEm - startEm) / 1000.0 + "s")

  }

  def writeAlignments(input1FilePath: String, input2FilePath: String, outputFilePath: String) {

    val outputFile = new java.io.FileWriter(outputFilePath)

    loopThroughFiles(input1FilePath, input2FilePath)((line1: String, line2: String, index: Int) => {

      val arr2WithIndex = line2 split " " zipWithIndex
      val size2 = arr2WithIndex.size

      for ((word2, index2) <- arr2WithIndex) {

        val arr1 = line1 split " "
        val size1 = arr1.size
        val arr1WithIndex = NULL +: arr1 zipWithIndex

        val (_, maxIndex) = arr1WithIndex.maxBy {
          case (word1, index1) => alignmentParams((index1, index2 + 1, size1, size2)) * translationParams(word1)(word2)
        }
        outputFile.write((index + 1) + " " + maxIndex + " " + (index2 + 1) + "\n")

      }

    })

    outputFile.close()

  }

  def writeParams(outputFilePath: String) {

    val outputFile = new java.io.FileWriter(outputFilePath)

    translationParams.foreach {
      case (word1, map) =>
        map.foreach { case (word2, prob) => outputFile.write(word1 + " " + word2 + " " + prob + "\n") }
    }

    outputFile.write("\n")

    alignmentParams.foreach {
      case ((index2, index1, size1, size2), prob) =>
        outputFile.write(index2 + " " + index1 + " " + size1 + " " + size2 + " " + prob + "\n")
    }

    outputFile.close()

  }

  def readParams(filePath: String) {

    val fileLines = Source.fromFile(filePath, "utf-8").getLines
    var reachedTransition = false

    for ((line, index) <- fileLines zipWithIndex) {
      if ((index + 1) % 200 == 0) println("line#:" + (index + 1))
      if (!line.trim.isEmpty) {
        val tokens = line split " "
        if (!reachedTransition) {
          val word1 = tokens(0)
          val word2 = tokens(1)
          val prob = tokens(2).toDouble
          if (translationParams.contains(word1))
            translationParams(word1) += (word2 -> prob)
          else
            translationParams(word1) = collection.mutable.Map(word2 -> prob)
        } else {
          val index2 = tokens(0).toInt
          val index1 = tokens(1).toInt
          val size1 = tokens(2).toInt
          val size2 = tokens(3).toInt
          val prob = tokens(4).toDouble
          alignmentParams((index2, index1, size1, size2)) = prob
        }
      } else {
        reachedTransition = true
      }
    }

  }

}